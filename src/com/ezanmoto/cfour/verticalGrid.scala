package com.ezanmoto.cfour

trait VerticalGrid[T] {
  def columnIsFull( column: Int ): Boolean
  def dropIntoColumn( value: T, column: Int ): VerticalGrid[T]
  def hasRowOf( n: Int ): Boolean
}

object VerticalGrid {
  def apply[T]( dimensions: (Int, Int) ) = new MapVGrid[T]( dimensions )
}

object VGridTest {
  def main( args: Array[ String ] ) = {
    val grid_ = VerticalGrid[Char]( 7, 7 ) dropIntoColumn ( 'X', 1 )
    var grid = grid_ dropIntoColumn ( 'O', 1 )
    grid = grid dropIntoColumn ( 'X', 2 )
    grid = grid dropIntoColumn ( 'O', 1 )
    println( grid dropIntoColumn ( 'O', 1 ) )
    for ( i <- 0 to 6 ) {
      grid = grid dropIntoColumn ( 'O', i )
      println( grid )
    }
    for ( i <- 0 to 4 ) {
      grid = grid dropIntoColumn ( 'X', 2 )
      println( grid )
    }
    println( grid columnIsFull 2 )
  }
}
