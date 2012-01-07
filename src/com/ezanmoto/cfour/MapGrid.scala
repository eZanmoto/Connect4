package com.ezanmoto.cfour

class MapGrid[T]( private val grid: Map[(Int, Int), T]
                , val width: Int, val height: Int ) extends Grid[T] {

  def this( dimensions: (Int, Int) ) =
    this( Map(), dimensions._1, dimensions._2 )

  def +( point: (Int, Int), value: T ): Grid[T] = {
    val row = point._1
    val column = point._2
    if ( inRange( row, column ) )
      new MapGrid[T]( grid + ( ( row, column ) -> value ), width, height )
    else
      throw new IllegalArgumentException( row + ", " + column
                                        + " out of bounds" )
  }

  private def inRange( point: (Int, Int) ) =
    point._1 >= 0 && point._1 < height && point._2 >= 0 && point._2 < height

  def hasEntryAt( point: (Int, Int) ) =
    if ( ( grid getOrElse( point, ( -1, -1 ) ) ) == ( -1, -1 ) )
      false
    else
      true

  def get( point: (Int, Int) ) =
    if ( this hasEntryAt point )
      ( grid get point ) get
    else
      throw new IllegalArgumentException( "No entry at '" + point + "'" )

  override def toString = {
    var string = " "
    for ( x <- 0 to width - 1 )
      string += "_ "
    string += "\n"
    for ( x <- 0 to width - 1 ) {
      for ( y <- 0 to height - 1 ) {
        string += "|"
        if ( hasEntryAt( x, y ) )
          string += grid.get( x, y ).get
        else
          string += "_"
      }
      string += "|\n"
    }
    string
  }
}
