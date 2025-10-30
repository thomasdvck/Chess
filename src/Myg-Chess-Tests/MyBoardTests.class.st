Class {
	#name : 'MyBoardTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyBoardTests >> testHighlightEmptyTarget [

	| board emptyTargets |
	board := MyChessBoard empty.
	emptyTargets := #().
	self shouldnt: [ board highLightTargets: emptyTargets ] raise: Error
]

{ #category : 'tests' }
MyBoardTests >> testUnhighlightEmptyTarget [

	| board emptyTargets |
	board := MyChessBoard empty.
	emptyTargets := #().
	self shouldnt: [ board unhighLightTargets: emptyTargets ] raise: Error
]
