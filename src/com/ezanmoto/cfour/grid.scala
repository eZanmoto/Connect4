package com.ezanmoto.cfour

trait Grid[T] {
  def width: Int
  def height: Int
  def +( point: (Int, Int), value: T ): Grid[T]
  def hasEntryAt( point: (Int, Int) ): Boolean
  def get( point: (Int, Int) ): T
}

object Grid {
  def apply[T]( dimensions: (Int, Int) ) = new MapGrid[T]( dimensions )
}

object GridTest {
  def main( args: Array[ String ] ) = {
    val grid_ = Grid[Char]( 7, 7 )
    var grid = grid_ + ( ( 0, 0 ), 'x' ) + ( ( 2, 2 ), 'x' ) + ( ( 1, 4 ), 'x' )
    println( grid )
    println( grid hasEntryAt ( 0, 0 ) )
  }
}
