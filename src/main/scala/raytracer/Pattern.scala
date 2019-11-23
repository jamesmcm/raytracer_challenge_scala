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

class TestPattern(val transform: Matrix) extends Pattern {
  val a: Colour = Colour(0, 0, 0)
  val b: Colour = Colour(0, 0, 0)

  def colourAt(p: RTTuple): Colour = {
    Colour(p.x, p.y, p.z)
  }
  def setTransform(t: Matrix): TestPattern = {
    new TestPattern(t)
  }
}

object TestPattern {
  def apply(): TestPattern = new TestPattern(Matrix.getIdentityMatrix(4))
}

class StripePattern(val a: Colour, val b: Colour, val transform: Matrix) extends Pattern {
  def colourAt(p: RTTuple): Colour = {
    if (math.floor(p.x + EPSILON).toInt % 2 === 0) a else b
  }
  def setTransform(t: Matrix): StripePattern = {
    new StripePattern(a, b, t)
  }
}

object StripePattern {
  def apply(a: Colour, b: Colour): StripePattern =
    new StripePattern(a, b, Matrix.getIdentityMatrix(4))
}

class GradientPattern(val a: Colour, val b: Colour, val transform: Matrix) extends Pattern {
  def colourAt(p: RTTuple): Colour = {
    a + ((b - a) * (p.x - math.floor(p.x + EPSILON)))
  }
  def setTransform(t: Matrix): GradientPattern = {
    new GradientPattern(a, b, t)
  }
}

object GradientPattern {
  def apply(a: Colour, b: Colour): GradientPattern =
    new GradientPattern(a, b, Matrix.getIdentityMatrix(4))
}

class RingPattern(val a: Colour, val b: Colour, val transform: Matrix) extends Pattern {
  def colourAt(p: RTTuple): Colour = {
    if (math.floor(math.sqrt(p.x * p.x + p.z * p.z) + EPSILON).toInt % 2 === 0) a else b
  }
  def setTransform(t: Matrix): RingPattern = {
    new RingPattern(a, b, t)
  }
}

object RingPattern {
  def apply(a: Colour, b: Colour): RingPattern = new RingPattern(a, b, Matrix.getIdentityMatrix(4))
}

class CheckeredPattern(val a: Colour, val b: Colour, val transform: Matrix) extends Pattern {
  def colourAt(p: RTTuple): Colour = {
    if ((math.floor(p.x + EPSILON) + math.floor(p.y + EPSILON) + math.floor(p.z + EPSILON)).toInt % 2 === 0)
      a
    else b
  }
  def setTransform(t: Matrix): CheckeredPattern = {
    new CheckeredPattern(a, b, t)
  }
}

object CheckeredPattern {
  def apply(a: Colour, b: Colour): CheckeredPattern =
    new CheckeredPattern(a, b, Matrix.getIdentityMatrix(4))
}

abstract class Pattern() {
  // TODO: Nested patterns
  val a: Colour
  val b: Colour
  val transform: Matrix

  val transform_inverse: Matrix = transform.inverse

  def colourAt(p: RTTuple): Colour
  def colourAtObject(o: SpaceObject, worldPoint: RTTuple): Colour = {
    val objectPoint: RTTuple  = o.transform_inverse.tupleMult(worldPoint)
    val patternPoint: RTTuple = transform_inverse.tupleMult(objectPoint)

    colourAt(patternPoint)
  }
}
