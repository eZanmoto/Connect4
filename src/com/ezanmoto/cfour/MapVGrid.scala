package com.ezanmoto.cfour

// Couldn't be bothered creating Connect4Grid so integrated that functionality
// into MapVGrid, i.e. hasRowOf( n )

class MapVGrid[T]( private val grid: Grid[T], val width: Int, val height: Int
                 , private val lastMove: Option[(Int, Int)] )
    extends VerticalGrid[T] {

  private val lastToken: Option[T] =
    if ( None == lastMove )
      None
    else
      Some( grid get ( lastMove get ) )

  def this( grid: Grid[T], lastMove: Option[(Int, Int)] ) =
    this( grid, grid width, grid height, lastMove )

  def this( dimensions: (Int, Int) ) =
    this( Grid( dimensions ), dimensions._1, dimensions._2, None )

  def columnIsFull( c: Int ) = grid hasEntryAt ( 0, c )

  def dropIntoColumn( v: T, c: Int ) =
    if ( columnIsFull( c ) )
      throw new IllegalArgumentException( "Cannot place '" + v + "' in " + c
                                        + ": column is full" )
    else {
      val point = ( topOfColumn( c ), c )
      new MapVGrid( grid + ( point, v ), Some( point ) )
    }

  private def topOfColumn( column: Int ): Int = {
    var row = height - 1
    while ( grid.hasEntryAt( row, column ) && row > 0 )
      row -= 1
    row
  }

  def hasRowOf( n: Int ) =
    hasHRowOf( n ) || hasVRowOf( n ) || hasNWRowOf( n ) || hasNERowOf( n )

  private def hasHRowOf  = hasDirRowOf( toWOf )( toEOf ) _
  private def hasVRowOf  = hasDirRowOf( toNOf )( toSOf ) _
  private def hasNWRowOf = hasDirRowOf( toNWOf )( toSEOf ) _
  private def hasNERowOf = hasDirRowOf( toNEOf )( toSWOf ) _

  private def hasDirRowOf( f1: Function[(Int, Int), Int] )
                         ( f2: Function[(Int, Int), Int] )( n: Int ) =
    ( f1( lastMove get ) + f2( lastMove get ) ) == n - 1

  private def toWOf  = toDirectionOf( -1,  0 ) _
  private def toEOf  = toDirectionOf(  1,  0 ) _
  private def toNOf  = toDirectionOf(  0,  1 ) _
  private def toSOf  = toDirectionOf(  0, -1 ) _
  private def toNWOf = toDirectionOf( -1,  1 ) _
  private def toNEOf = toDirectionOf(  1,  1 ) _
  private def toSWOf = toDirectionOf( -1, -1 ) _
  private def toSEOf = toDirectionOf(  1, -1 ) _

  private def toDirectionOf( direction: (Int, Int) )
                           ( point: (Int, Int) ): Int = {
    val p = ( point._1 + direction._1, point._2 + direction._2 )
    if ( ( grid hasEntryAt p ) && ( grid get p ) == ( lastToken get ) )
      toDirectionOf( direction )( p ) + 1
    else
      0
  }

  override def toString = grid toString
}
