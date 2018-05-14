val level =
       """ST
          |oo
        |oo""".stripMargin

val level2 =
  """ooo-------
    |oSoooo----
    |ooooooooo-
    |-ooooooooo
    |-----ooToo
    |------ooo-""".stripMargin


private lazy val vector: Vector[Vector[Char]] =
  Vector(
    level.
      split("\n").
    map(str => Vector(str: _*)): _*)


val pepito = Vector(level2.
  split("\n").
map(str => Vector(str: _*)):_*)

def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = x => levelVector(x.row)(x.col) != '-'


case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

terrainFunction(pepito)(Pos(1,15))

