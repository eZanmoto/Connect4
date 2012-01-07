package com.ezanmoto.cfour

import java.util.Scanner

trait Connect4 {
  def hasWinner: Boolean
  def winner: Char
  def columnIsFull( column: Int ): Boolean
  def placeToken( name: Char, column: Int ): Connect4
}

object C4 {

  val maxTurns = 7 * 7

  val in = new Scanner( System.in )

  private def takeTurnOf( token: Char, game: Connect4 ): Connect4 = {
    var column = -1
    println( "Enter column: " )
    while ( -1 == column ) {
      if ( in hasNextInt ) {
        val c = in.nextInt
        if ( c < 1 || c > 7 )
          println( "Column must be in range 1..7" )
        else if ( game columnIsFull c )
          println( "Column " + c + " is full" )
        else
          column = c
      } else
        while ( ! in.hasNextInt ) {
          val toSkip: String = in next
        }
    }
    game placeToken( token, column )
  }

  def main( args: Array[String] ) = {
    var game: Connect4 = new Connect4_1
    var turn = 0
    while ( ! game.hasWinner && turn < maxTurns ) {
      val token = if ( turn % 2 == 0 ) 'x' else 'o'
      game = takeTurnOf( token, game )
      println( game )
      turn += 1
    }
    if ( maxTurns == turn )
      println( "Match was a draw" )
    else
      println( "'" + game.winner + "' won the game" )
  }
}

class Connect4_1( private val grid: VerticalGrid[Char], val hasWinner: Boolean
                , private val winnerName: Char ) extends Connect4 {

  def this( grid: VerticalGrid[Char] ) = this( grid, false, '?' )

  def this() = this( VerticalGrid( 7, 7 ) )

  def winner =
    if ( hasWinner )
      winnerName
    else
      throw new RuntimeException( "Game has no winner yet" )

  def columnIsFull( c: Int ) = grid columnIsFull ( c - 1 )

  def placeToken( name: Char, column: Int ) =
    if ( column >= 1 && column <= 7 ) {
      val grid = this.grid dropIntoColumn( name, column - 1 )
      val hasWinner = grid hasRowOf 4
      new Connect4_1( grid, hasWinner, if ( hasWinner ) name else '?' )
    } else
      throw new RuntimeException( "Column must be in 1..7, got " + column )

  override def toString = grid toString
}
