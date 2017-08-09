(* ::Package:: *)

BeginPackage["CastlevaniaDemo`"];


playLevel::usage="Play a modified version of the first stage of the classic Castlevania game.";


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*global settings  *)


screenWidth=1.396;
mapEdge=3.4;
scrollPos=0;
playAgain=False;


(* ::Subsection::Closed:: *)
(*Sprites*)


background = Import[FileNameJoin[{NotebookDirectory[],"Sprites","LevelMap.png"}]];
controls = Import[FileNameJoin[{NotebookDirectory[],"Sprites","Controls2.png"}]];
whippingLeft = Table[Import[FileNameJoin[{NotebookDirectory[],"Sprites","whip"<>ToString[i]<>".png"}]],{i,4}];
crouchWhippingLeft = Table[Import[FileNameJoin[{NotebookDirectory[],"Sprites","crouchWhip"<>ToString[i]<>".png"}]],{i,4}];
walkingLeft = Table[Import[FileNameJoin[{NotebookDirectory[],"Sprites","walking"<>ToString[i]<>".png"}]],{i,4}];
crouchLeft = crouchWhippingLeft[[-1]];
whippingRight = ImageReflect[#,Right]& /@ whippingLeft;
crouchWhippingRight = ImageReflect[#,Right]& /@ crouchWhippingLeft;
walkingRight = ImageReflect[#,Right]& /@ walkingLeft;
crouchRight = ImageReflect[crouchLeft,Right];
ghoulLeft = Table[Import[FileNameJoin[{NotebookDirectory[],"Sprites","ghoul"<>ToString[i]<>".png"}]],{i,1,2,1}];
ghoulRight = ImageReflect[#,Right]& /@ ghoulLeft;
enemyDeath = Table[Import[FileNameJoin[{NotebookDirectory[],"Sprites","enemyDeath"<>ToString[i]<>".png"}]],{i,2}];
playerDeathLeft = Table[Import[FileNameJoin[{NotebookDirectory[],"Sprites","playerDeath"<>ToString[i]<>".png"}]],{i,3}];
playerDeathRight = ImageReflect[#,Right]& /@ playerDeathLeft;


(* ::Subsection::Closed:: *)
(*Music and Sound Effects*)


music=Import[FileNameJoin[{NotebookDirectory[],"Sounds","VamKill.mid"}]];


Do[sound[i]=Import[FileNameJoin[{NotebookDirectory[],"Sounds","0"<>ToString[i]<>".wav"}],"Sound"],{i,1,9}];
Do[sound[i]=Import[FileNameJoin[{NotebookDirectory[],"Sounds",ToString[i]<>".wav"}],"Sound"],{i,10,37}];


whipSound=sound[10];
hitSound=sound[20];


(* ::Subsection:: *)
(*Sprite actions*)


(* ::Subsubsection::Closed:: *)
(*Hit box check*)


impactQ = Compile[{{box1,_Real,2},{box2,_Real,2}},
	(* if one hitbox is on the left side of the other *)
	If[box1[[2,1]] < box2[[1,1]] || box1[[1,1]] > box2[[2,1]], Return[False]];
	
	(* if one hitbox is above the other *)
	If[box1[[2,2]] < box2[[1,2]] || box1[[1,2]] > box2[[2,2]], Return[False]];
	
	True
];


(* ::Subsubsection:: *)
(*Character*)


(* use Module to create "static" variables *)
Module[{playerFacingRight=True, playerCrouched=False, playerX=0.288, playerY=0.282, 
playerXvel=0.2, scrollSpeed=0.2, im=walkingRight[[1]], 
playerAnimationCounter=1, playerFrame=1, walkAnimationDelay=25,
grav=0.5, jumpVel=0.5, playerYvel=0, jumpCounter=0, previousJumpState=False,
whipFrame=1, whipAnimationCounter=1, previousWhipState=False, whipping=False, whipAnimationDelay=8,
whipStandLeftBox,whipStandRightBox,whipCrouchedLeftBox,whipCrouchedRightBox,
crouchedBox,standingBox,playerBox,whipSoundCheck=True},

(* define hit boxes *)
whipStandLeftBox = {{-0.2`,0.02`},{-0.06`,0.07`}};
whipStandRightBox = {{0.06`,0.02`},{0.2`,0.07`}};
whipCrouchedLeftBox = {{-0.204`,-0.02`},{-0.052`,0.034`}};
whipCrouchedRightBox = {{0.052`,-0.02`},{0.204`,0.034`}};
crouchedBox = {{-0.02`,-0.06`},{0.02`,0.038`}};
standingBox = {{-0.02`,-0.06`},{0.02`,0.086`}};
playerBox = standingBox + {{playerX,playerY},{playerX,playerY}};


(* function to update the whip hitbox and animation while in standing position *)
Attributes[standWhipUpdate] = {HoldAll};
standWhipUpdate[whipBox_] := (
	whipAnimationCounter += 1;
	If[Mod[whipAnimationCounter,whipAnimationDelay]==0,
		whipAnimationCounter=1;
		Switch[whipFrame,
			(* at the end of the last frame normal walking starts; whip hitbox remains off screen *)
			4, 
			whipping=False; 
			whipSoundCheck=True;
			playerFrame=whipFrame=1; 
			whipBox = {{-10,-10},{-5,-5}};
			im = If[playerFacingRight, walkingRight, walkingLeft][[playerFrame]],
			
			(* at the end of the 2nd frame, whipbox becomes active *)
			2,
			whipFrame += 1; 
			whipBox = If[playerFacingRight, whipStandRightBox, whipStandLeftBox] + {{playerX,playerY},{playerX,playerY}};
			im = If[playerFacingRight, whippingRight, whippingLeft][[whipFrame]],
			
			(* frames 1 and 3; whip hitbox is positioned off screen *)
			_,
			whipFrame += 1; 
			whipBox = {{-10,-10},{-5,-5}};
			im = If[playerFacingRight, whippingRight, whippingLeft][[whipFrame]]
		];
	]
);


(* function to update the whip hitbox and animation while crouched *)
Attributes[crouchWhipUpdate] = {HoldAll};
crouchWhipUpdate[whipBox_] := (
	whipAnimationCounter += 1;
	If[Mod[whipAnimationCounter,whipAnimationDelay]==0,
		whipAnimationCounter=1;
		Switch[whipFrame,
			(* last frame starts normal walking; whip hitbox is positioned off screen *)
			4,
			whipping=False; 
			whipSoundCheck=True;
			playerFrame=whipFrame=1; 
			whipBox = {{-10,-10},{-5,-5}};
			im = If[playerFacingRight, crouchRight, crouchLeft],
			
			(* 2nd frame is only frame where whipbox is active *)
			2,
			whipFrame += 1; 
			whipBox = If[playerFacingRight, whipCrouchedRightBox, whipCrouchedLeftBox] + {{playerX,playerY},{playerX,playerY}};
			im = If[playerFacingRight, crouchWhippingRight, crouchWhippingLeft][[whipFrame]],
			
			(* frames 1 and 3; whip hitbox is positioned off screen *)
			_,
			whipFrame += 1; 
			whipBox = {{-10,-10},{-5,-5}};
			im = If[playerFacingRight, crouchWhippingRight, crouchWhippingLeft][[whipFrame]]
		];			
	]
);


(* function to move character and screen to the right *)
Attributes[moveRight] = {HoldRest};
moveRight[csX_, fps_] := (
	(* face player in direction of movement *)
	If[!playerFacingRight, playerFacingRight=True];
	
	(* update player sprite and screen position *)
	With[{p=playerX+(playerXvel/fps)*csX}, If[p < 3.55, playerX=p]];
	If[scrollPos < 2.7 && ((scrollPos+screenWidth)/2.4 < playerX), scrollPos+=(scrollSpeed/fps)]
);


(* function to move character and screen to the left *)
Attributes[moveLeft] = {HoldRest};
moveLeft[csX_, fps_] := (
	(* face player in direction of movement *)
	If[playerFacingRight, playerFacingRight=False];
	
	(* update player sprite and screen position *)
	With[{p=playerX+(playerXvel/fps)*csX}, If[0.07 < p, playerX=p]];
	If[scrollPos > 0 && (scrollPos+screenWidth/2.3 > playerX), scrollPos-=(scrollSpeed/fps)]
);


(* main character function *)
Attributes[character] = {HoldRest};
character[{csX_,csY_,csB_, csB2_}, enemyBox_, whipBox_, fps_, playerDead_] := Module[{},
	(* check for impact with enemy *)
	If[
		AnyTrue[{enemyBox[1],enemyBox[2],enemyBox[3],enemyBox[4],enemyBox[5],
			enemyBox[6],enemyBox[7],enemyBox[8],enemyBox[9],enemyBox[10]},
			impactQ[playerBox,#]&
		], 
		playerDead = True; playerCrouched=True;
		playerBox = {{1,1},{1,1}};
		playerFrame=1;
		im = If[playerFacingRight, playerDeathRight, playerDeathLeft][[playerFrame]];
		playerAnimationCounter=1;
	];

	If[!playerDead,
	
	(* check state of jump button *)
	Switch[{previousJumpState,csB},
		{False,True}, If[jumpCounter<2,jumpCounter+=1;playerYvel=(jumpVel/fps);previousJumpState=True],
		{True,False}, previousJumpState=False;
	];
	
	(* check state of whip button *)
	Switch[{previousWhipState,csB2},
		{False,True}, 
			whipping=True; 
			If[whipSoundCheck, EmitSound[whipSound]; whipSoundCheck=False];
			(* immediately update animation frame *)
			im = Switch[{playerFacingRight,playerCrouched},
				{True,True}, crouchWhippingRight, 
				{False,True}, crouchWhippingLeft,
				{True,False}, whippingRight, 
				{False,False}, whippingLeft
				][[whipFrame]];
			previousWhipState=True,
		{True,False}, 
			previousWhipState=False
	];
	
	];
	
	(* put player in gravity; player cannot pass through the ground *)
	playerYvel-=(grav/fps/fps);
	With[{p=playerY+playerYvel},
		If[p > 0.282, playerY = p, jumpCounter=0; playerYvel=0; playerY = 0.282]
	];	
	
	If[!playerDead,
	
	If[jumpCounter>0,
	(* while in the air *)
		playerFrame=1; playerCrouched=False;
		Which[
			csX==1, moveRight[csX, fps],
			csX==-1, moveLeft[csX, fps]
		];
		If[whipping,
			(* if whipping, player is in standing position *)
			standWhipUpdate[whipBox];
			(* whipping in the air is unique as movement is allowed;
			   hit boxes need to update with the player's movement *)
			playerBox = standingBox;
			If[whipFrame==3, whipBox = If[playerFacingRight, whipStandRightBox, whipStandLeftBox] + {{playerX,playerY},{playerX,playerY}}],
			
			(* if not whipping, the legs tuck midway through the jump *)
			If[-(jumpVel/fps)/1.5 < playerYvel < (jumpVel/fps)/1.5, 
				playerBox = crouchedBox;
				im = If[playerFacingRight, crouchRight, crouchLeft],
				
				playerBox = standingBox;
				im = If[playerFacingRight, walkingRight, walkingLeft][[playerFrame]];
			];
		],
		
	(* while on the ground *)
		If[whipping,
		(* if whipping, side-movement is not allowed *)
			If[playerCrouched, 
				crouchWhipUpdate[whipBox];
				playerBox = crouchedBox, 
				standWhipUpdate[whipBox];
				playerBox = standingBox
			],
			
		(* if not whipping, then movement can occur *)
			Which[
			(* walking to the right *)
				csX==1 && (csY==0 || csY==1),
				moveRight[csX, fps];
				playerCrouched=False;
			
				(* animate character sprite *)
				playerAnimationCounter += 1;
				If[Mod[playerAnimationCounter,walkAnimationDelay]==0,
					playerAnimationCounter=1;
					im=walkingRight[[If[playerFrame==4,playerFrame=1,++playerFrame]]]
				],
			
			(* walking to the left *)
				csX==-1 && (csY==0 || csY==1), 
				moveLeft[csX, fps];
				playerCrouched=False;
				
				(* animate character sprite *)
				playerAnimationCounter += 1;
				If[Mod[playerAnimationCounter,walkAnimationDelay]==0,
					playerAnimationCounter=1; 
					im=walkingLeft[[If[playerFrame==4,playerFrame=1,++playerFrame]]]
				],
			
			(* crouched to the right *)
				csX==1 && csY==-1,
				If[!playerFacingRight, playerFacingRight=True];
				playerCrouched=True;
				playerAnimationCounter=9;
				im=crouchRight,
			
			(* crouched to the left *)
				csX==-1 && csY==-1,
				If[playerFacingRight, playerFacingRight=False];
				playerCrouched=True;
				playerAnimationCounter=9;
				im=crouchLeft,
			
			(* crouch down *)
				csX==0 && csY==-1,
				If[!playerCrouched, 
					playerCrouched=True; 
					im = If[playerFacingRight, crouchRight, crouchLeft]
				],
				
			(* stand up *)	
				csX==0 && csY==1,
				playerFrame=1;
				If[playerCrouched, 
					playerCrouched=False; 
					im = If[playerFacingRight, walkingRight, walkingLeft][[playerFrame]]
				]
			];
			
			(*update non-whipping player hitbox*)
			playerBox = If[playerCrouched, crouchedBox, standingBox]; 
		]
	];
	
	playerBox += {{playerX,playerY},{playerX,playerY}};
	
	,
	(* if dying *)
		(* animate character death *)
		playerAnimationCounter += 1;
		If[Mod[playerAnimationCounter,walkAnimationDelay]==0,
			playerAnimationCounter=1;
			If[playerFrame < 3, playerFrame++];
			im = If[playerFacingRight, playerDeathRight, playerDeathLeft][[playerFrame]];
		]
	];
	
	
	Inset[im, {playerX,playerY}, Scaled[{0.5,0.5}], 0.562]
]

] (* end of module that creates "static" variables *)


(* ::Subsubsection::Closed:: *)
(*Enemies*)


initializeEnemy[{}, _] := Null

Attributes[createEnemy] = {HoldRest};
createEnemy[index_Integer, whipBox_, enemyBox_, tsf_, score_, enemyReady_] := Module[
{xPos, yPos, xVel, yVel, im, speedTrigger=False, dying=False, size,
enemyAnimationCounter=1, enemyFrame=1, enemyAnimationDelay=25},

enemyBox = {{0,0},{0,0}};

(* ghoul facing right *)
	initializeEnemy[index, "ghoulRight"] := (
		xPos=-0.1; xVel=0.2; 
		yPos=0.29; yVel=0.;
		size = 0.122;
		speedTrigger=True; tsf-=2;
		im = ghoulRight[[1]];
		enemyReady[index] = False;
		
		enemy[index] :=  (
			If[!dying,
			(* if alive *)
				(* update position and enemy hit box *)
				xPos += xVel/tsf; 
				enemyBox = {{-0.037,-0.075}+{xPos,yPos},{0.029,0.081}+{xPos,yPos}};

				(* update enemy animation *)
				enemyAnimationCounter += 1;
				If[Mod[enemyAnimationCounter, enemyAnimationDelay]==0,
					enemyAnimationCounter=1; 
					im=ghoulRight[[If[enemyFrame==2,enemyFrame=1,++enemyFrame]]]
				];
				
				(* if hit with whip or exited map, start dying animation *)
				If[impactQ[whipBox,enemyBox], 
					dying = True; EmitSound[hitSound]; score++; 
					enemyBox = {{0,0},{0,0}};
					size = 0.035;
					enemyFrame=1;
					im = enemyDeath[[enemyFrame]];
					enemyAnimationCounter=1;
				];
				If[xPos > 3.8, 
					enemyBox = {{0,0},{0,0}};
					enemyFrame = 1;
					xPos = -10; xVel = 0;
					If[speedTrigger, tsf+=2; speedTrigger = False]; 
					enemyReady[index]=True;
				],
				
			(* if dead *)
				(* update death animation *)
				enemyAnimationCounter += 1;
				If[Mod[enemyAnimationCounter, enemyAnimationDelay]==0,
					enemyAnimationCounter=1;
					enemyFrame += 1;
					If[enemyFrame < 3,
						im=enemyDeath[[enemyFrame]],
						
						(* after death animation, position enemy off screen, update score *)
						dying = False; enemyFrame = 1;
						xPos = -10; xVel = 0;
						If[speedTrigger, tsf+=2; speedTrigger = False]; 
						enemyReady[index]=True;
					]
				];
									
			];
			
			{
				(*{EdgeForm[Directive[Red,Thick]],FaceForm[None],Rectangle@@enemyBox},*)
				Inset[im, {xPos,yPos}, Scaled[{0.5,0.5}], size]
			}
		);
	);

(* ghoul facing left *)
	initializeEnemy[index, "ghoulLeft"] := (
		xPos=3.8; xVel=-0.2; 
		yPos=0.29; yVel=0.;
		size = 0.122;
		speedTrigger=True; tsf-=2;
		im = ghoulLeft[[1]];
		enemyReady[index] = False;
		
		enemy[index] :=  (
			If[!dying,
			(* if alive *)
				(* update position and enemy hit box *)
				xPos += xVel/tsf; 
				enemyBox = {{-0.037,-0.075}+{xPos,yPos},{0.029,0.081}+{xPos,yPos}};

				(* update enemy animation *)
				enemyAnimationCounter += 1;
				If[Mod[enemyAnimationCounter, enemyAnimationDelay]==0,
					enemyAnimationCounter=1; 
					im=ghoulLeft[[If[enemyFrame==2,enemyFrame=1,++enemyFrame]]]
				];
				
				(* if hit with whip or exited map, start dying animation *)
				If[impactQ[whipBox,enemyBox], 
					dying = True; EmitSound[hitSound]; score++;
					enemyBox = {{0,0},{0,0}};
					size = 0.035;
					enemyFrame=1;
					im = enemyDeath[[enemyFrame]];
					enemyAnimationCounter=1;
				];
				If[xPos < 0.1, 
					enemyBox = {{0,0},{0,0}};
					enemyFrame = 1;
					xPos = -10; xVel = 0;
					If[speedTrigger, tsf+=2; speedTrigger = False]; 
					enemyReady[index]=True;
				],
				
			(* if dead *)
				(* update death animation *)
				enemyAnimationCounter += 1;
				If[Mod[enemyAnimationCounter, enemyAnimationDelay]==0,
					enemyAnimationCounter=1;
					enemyFrame += 1;
					If[enemyFrame < 3,
						im=enemyDeath[[enemyFrame]],
						
						(* after death animation, position enemy off screen, update score *)
						dying = False; enemyFrame = 1;
						xPos = -10; xVel = 0;
						If[speedTrigger, tsf+=2; speedTrigger = False]; 
						enemyReady[index]=True;
					]
				];
									
			];
			
			{
				(*{EdgeForm[Directive[Red,Thick]],FaceForm[None],Rectangle@@enemyBox},*)
				Inset[im, {xPos,yPos}, Scaled[{0.5,0.5}], size]
			}
		);
	)
]


(* ::Subsection:: *)
(*main*)


playLevel[] := Deploy@DynamicModule[{whipBox={{-10,-10},{-5,-5}}, fps=50, 
enemyBox, playerDead=False, score=0, enemyReady, previousButtonState=False, pressed=False, enemyCounter=1, 
enemyVariate=RandomVariate[NormalDistribution[100,30]]},
(* start background music *)
(*EmitSound[music];*)

(* use PaneSelector to toggle between pause screen and active game *)
PaneSelector[
{
True -> 
	Column[{
	Graphics[
		{
		Inset[background, {0,0}, Scaled[{0,0}], 4.19],
		Dynamic[Inset[Style[score,Red,Bold,27], {scrollPos+screenWidth-0.1,0.9}]],
		Dynamic[
			If[playerDead, 
				If[ControllerState["B7"], 
					playerDead=False; score=0; character[{1,-1,False,True}, enemyBox, whipBox, fps, playerDead]
				];
				Inset[Panel[Style["   Play again? \nPress Restart Button.",Red,Bold,27],Background->Black], {scrollPos+screenWidth*0.5,0.5}],
				
				{}
			]
		],
		Dynamic[{
			If[(enemyCounter += 1) > enemyVariate, 
				enemyCounter=1; enemyVariate=RandomVariate[NormalDistribution[100,30]];
				initializeEnemy[ 
					First[Pick[{1,2,3,4,5,6,7,8,9,10}, enemyReady /@ {1,2,3,4,5,6,7,8,9,10}], {}], 
					RandomChoice[{"ghoulLeft","ghoulRight"}] 
				]
			];
			enemy[1],enemy[2],enemy[3],enemy[4],enemy[5],
			enemy[6],enemy[7],enemy[8],enemy[9],enemy[10],
			character[
				{
				ControllerState["X3"],ControllerState["Y3"],
				ControllerState["B1"],ControllerState["B3"]
				},
				enemyBox, whipBox, fps, playerDead
			]
		}]
		}, 
		Frame -> False,
		ImageSize -> Large,
		PlotRange -> {{Dynamic[0+scrollPos],Dynamic[screenWidth+scrollPos, TrackedSymbols:>{scrollPos}]}, {0,1}},
		PlotRangePadding -> None,
		PlotRangeClipping -> True,
		ImagePadding -> 1
	](*,
	LabeledSlider[Dynamic[fps],{1,72}]*)
	}],

False -> controls

},

Dynamic[
	Switch[{previousButtonState,ControllerState["B8"]},
		{False,True}, If[pressed,pressed=False,pressed=True]; previousButtonState=True,
		{True,False}, previousButtonState=False
	];
	pressed
],
ImageSize -> Automatic
](* end PaneSelector *),

Initialization :> (
	Do[With[{i=i}, createEnemy[i, whipBox, enemyBox[i], fps, score, enemyReady]], {i,1,10}];
	enemy[1]=enemy[2]=enemy[3]=enemy[4]=enemy[5]=enemy[6]=enemy[7]=enemy[8]=enemy[9]=enemy[10] = {};
	enemyReady[1]=enemyReady[2]=enemyReady[3]=enemyReady[4]=enemyReady[5] = True;
	enemyReady[6]=enemyReady[7]=enemyReady[8]=enemyReady[9]=enemyReady[10] = True;
)	
]


End[];


EndPackage[]


(* ::Subsection:: *)
(*Notes*)
