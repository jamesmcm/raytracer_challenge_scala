// Copyright (C) 2019 James McMurray
//
// raytracer_challenge is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// raytracer_challenge is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with raytracer_challenge  If not, see <http://www.gnu.org/licenses/>.

package raytracer


class Canvas(val width: Int, val height: Int){
  // TODO: Avoid var
  var pixels: Array[Array[Colour]] = Array.fill(height)(Array.fill(width)(Colour(0,0,0)))
  val maxcol: Int = 255

  def writePixel(x: Int, y: Int, c: Colour): Unit = {
    // TODO: return new canvas
    pixels(y)(x) = c
  }

  def pixelAt(x: Int, y: Int): Colour = pixels(y)(x)

  def scaleAndClamp(x: Double): Int = {
    math.round(x * maxcol).toInt match {
      case x if x > maxcol => maxcol
      case x if x < 0 => 0
      case x => x
    }
  }

  def toPPM: String = {
    s"P3\n$width $height\n$maxcol\n" + pixels.map(
      (x: Array[Colour]) => x.map(
        (c: Colour) => Seq(c.red, c.green, c.blue).map(
          scaleAndClamp
        ).mkString(" ")
      ).mkString(" ")
    ).mkString("\n")
  }

  def cutTo70Chars(s: String): String = {
    s.length match {
      case x if x > 70 => s
      case _ => s
    }
  }
}
