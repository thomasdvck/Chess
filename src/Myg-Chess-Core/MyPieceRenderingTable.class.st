Class {
	#name : 'MyPieceRenderingTable',
	#superclass : 'Object',
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'as yet unclassified' }
MyPieceRenderingTable class >> symbolFor: pieceClass pieceColor: pieceColor squareColor: squareColor [

	| pieceKey squareKey |
	pieceKey := pieceColor isBlack
		            ifTrue: [ #black ]
		            ifFalse: [ #white ].
	squareKey := squareColor isBlack
		             ifTrue: [ #black ]
		             ifFalse: [ #white ].
	^ ((self table at: pieceClass) at: pieceKey) at: squareKey
]

{ #category : 'as yet unclassified' }
MyPieceRenderingTable class >> table [

	| table |
	table := Dictionary new.

	table at: MyKnight put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'N';
					  at: #black put: 'n';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'M';
				  at: #black put: 'm';
				  yourself);
			 yourself).

	table at: MyBishop put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'B';
					  at: #black put: 'b';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'V';
				  at: #black put: 'v';
				  yourself);
			 yourself).

	table at: MyRook put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'R';
					  at: #black put: 'r';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'T';
				  at: #black put: 't';
				  yourself);
			 yourself).

	table at: MyQueen put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'Q';
					  at: #black put: 'q';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'W';
				  at: #black put: 'w';
				  yourself);
			 yourself).

	table at: MyKing put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'K';
					  at: #black put: 'k';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'L';
				  at: #black put: 'l';
				  yourself);
			 yourself).

	table at: MyPawn put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'P';
					  at: #black put: 'p';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'O';
				  at: #black put: 'o';
				  yourself);
			 yourself).

	table at: MyNilPiece put: (Dictionary new
			 at: #white put: (Dictionary new
					  at: #white put: 'z';
					  at: #black put: 'x';
					  yourself);
			 at: #black put: (Dictionary new
				  at: #white put: 'z';
				  at: #black put: 'x';
				  yourself);
			 yourself).

	^ table
]
