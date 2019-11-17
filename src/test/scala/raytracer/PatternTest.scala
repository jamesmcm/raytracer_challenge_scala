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


import org.scalatest.FunSuite

class PatternTest extends FunSuite {
  test("Pattern.test_create") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0))
    assert(p.a === Colour(1,1,1) && p.b === Colour(0,0,0))
  }

  test("Pattern.test_stripe_y") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0))
    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0,1,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0,2,0)) === Colour(1,1,1)
    )
  }
  test("Pattern.test_stripe_z") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0))
    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0,0,1)) === Colour(1,1,1) &&
      p.colourAt(Point(0,0,2)) === Colour(1,1,1)
    )
  }
  test("Pattern.test_stripe_x") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0))
    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0.9,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(1,0,0)) === Colour(0,0,0) &&
      p.colourAt(Point(-0.1,0,0)) === Colour(0,0,0) &&
      p.colourAt(Point(-1,0,0)) === Colour(0,0,0) &&
      p.colourAt(Point(-1.1,0,0)) === Colour(1,1,1)
    )
  }
  test("Pattern.test_stripe_object_transform") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0))
    val o: Sphere = Sphere.unitSphere().setTransform(Scaling(2,2,2))

    val c: Colour = p.colourAtObject(o, Point(1.5,0,0))
    assert(c === Colour(1,1,1))
  }
  test("Pattern.test_stripe_pattern_transform") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0)).setTransform(Scaling(2,2,2))
    val o: Sphere = Sphere.unitSphere()

    val c: Colour = p.colourAtObject(o, Point(1.5,0,0))
    assert(c === Colour(1,1,1))
  }
  test("Pattern.test_stripe_object_pattern_transform") {
    val p: Pattern = StripePattern(Colour(1,1,1), Colour(0,0,0)).setTransform(Translation(0.5, 0, 0))
    val o: Sphere = Sphere.unitSphere().setTransform(Scaling(2,2,2))

    val c: Colour = p.colourAtObject(o, Point(2.5,0,0))
    assert(c === Colour(1,1,1))
  }
  test("Pattern.test_gradient") {
    val p: Pattern = GradientPattern(Colour(1,1,1), Colour(0,0,0))

    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0.25,0,0)) === Colour(0.75,0.75,0.75) &&
      p.colourAt(Point(0.5,0,0)) === Colour(0.5,0.5,0.5) &&
      p.colourAt(Point(0.75,0,0)) === Colour(0.25,0.25,0.25))
  }

  test("Pattern.test_ring") {
    val p: Pattern = RingPattern(Colour(1,1,1), Colour(0,0,0))

    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(1,0,0)) === Colour(0,0,0) &&
      p.colourAt(Point(0,0,1)) === Colour(0,0,0) &&
      p.colourAt(Point(0.708,0,0.708)) === Colour(0,0,0))
  }
  test("Pattern.test_checkered_x") {
    val p: Pattern = CheckeredPattern(Colour(1,1,1), Colour(0,0,0))

    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0.99,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(1.01,0,0)) === Colour(0,0,0))
  }
  test("Pattern.test_checkered_y") {
    val p: Pattern = CheckeredPattern(Colour(1,1,1), Colour(0,0,0))

    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0,0.99,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0,1.01,0)) === Colour(0,0,0))
  }
  test("Pattern.test_checkered_z") {
    val p: Pattern = CheckeredPattern(Colour(1,1,1), Colour(0,0,0))

    assert(p.colourAt(Point(0,0,0)) === Colour(1,1,1) &&
      p.colourAt(Point(0,0,0.99)) === Colour(1,1,1) &&
      p.colourAt(Point(0,0,1.01)) === Colour(0,0,0))
  }

  test("Pattern.test_pattern_identity") {
    val p: Pattern = TestPattern()

    assert(p.transform === Matrix.getIdentityMatrix(4))
  }
  test("Pattern.test_creation_pattern_transform") {
    val p: Pattern = TestPattern().setTransform(Translation(1,2,3))

    assert(p.transform === Translation(1,2,3))
  }
  test("Pattern.test_object_transform") {
    val p: Pattern = TestPattern()
    val s: Sphere = Sphere.unitSphere().setTransform(Scaling(2,2,2))
    val c: Colour = p.colourAtObject(s, Point(2,3,4))

    assert(c === Colour(1,1.5,2))
  }
  test("Pattern.test_pattern_transform") {
    val p: Pattern = TestPattern().setTransform(Scaling(2,2,2))
    val s: Sphere = Sphere.unitSphere()
    val c: Colour = p.colourAtObject(s, Point(2,3,4))

    assert(c === Colour(1,1.5,2))
  }
  test("Pattern.test_pattern_object_transform") {
    val p: Pattern = TestPattern().setTransform(Translation(0.5, 1, 1.5))
    val s: Sphere = Sphere.unitSphere().setTransform(Scaling(2,2,2))
    val c: Colour = p.colourAtObject(s, Point(2.5,3,3.5))

    assert(c === Colour(0.75, 0.5, 0.25))
  }

}
