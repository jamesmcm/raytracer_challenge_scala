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

import cats.implicits._

class StripePattern(val a: Colour, val b: Colour, val transform: Matrix) extends Pattern {
  def colourAt(p: RTTuple): Colour = {
    if (math.floor(p.x).toInt % 2 === 0) a else b
  }
  def setTransform(t: Matrix): StripePattern = {
    new StripePattern(a, b, t)
  }
}

object StripePattern {
  def apply(a: Colour, b: Colour): StripePattern = new StripePattern(a, b, Matrix.getIdentityMatrix(4))
}

abstract class Pattern(){
  // TODO: Nested patterns
  val a: Colour
  val b: Colour
  val transform: Matrix

  def colourAt(p: RTTuple): Colour
  def colourAtObject(o: SpaceObject, worldPoint: RTTuple): Colour = {
    val objectPoint: RTTuple = o.transform.inverse.tupleMult(worldPoint)
    val patternPoint: RTTuple = transform.inverse.tupleMult(objectPoint)

    colourAt(patternPoint)
  }
}