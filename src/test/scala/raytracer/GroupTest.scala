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

class GroupTest extends FunSuite {
  test("Group.test_creation") {
    val g: Group    = Group()

    assert(g.transform === Matrix.getIdentityMatrix(4) && g.isEmpty)
  }
  test("Group.test_shape") {
    val s: SpaceObject    = Sphere.unitSphere()

    assert(s.parent.isEmpty)
  }
  test("Group.test_add_parent") {
    val s: SpaceObject    = Sphere.unitSphere()
    val g: Group    = Group().addChild(s)

    assert(!g.isEmpty && g.contains(s) && s.parent === Some(g))
  }
  test("Group.test_intersect_empty") {
    val g: Group    = Group()
    val r: Ray    = Ray(Point(0,0,0), Vector(0,0,1))

    val xs: Seq[Intersection] = g.localIntersect(r)

    assert(g.isEmpty)
  }
  test("Group.test_intersect_group") {
    val r: Ray    = Ray(Point(0,0,-5), Vector(0,0,1))
    val s1: Sphere = Sphere.unitSphere()
    val s2: Sphere = Sphere.unitSphere().setTransform(Translation(0,0,-3))
    val s3: Sphere = Sphere.unitSphere().setTransform(Translation(5,0,0))
    val g: Group    = Group().addChild(s1).addChild(s2).addChild(s3)

    val xs: Seq[Intersection] = g.localIntersect(r)

    assert(xs.length === 4 && xs(0).shape === s2 && xs(1).shape === s2 && xs(2).shape === s1 && xs(3).shape === s1)
  }
  test("Group.test_transform_group") {
    val r: Ray    = Ray(Point(10,0,-10), Vector(0,0,1))
    val s1: Sphere = Sphere.unitSphere().setTransform(Translation(5,0,0))
    val g: Group    = Group().addChild(s1).setTransform(Scaling(2,2,2))

    val xs: Seq[Intersection] = g.intersect(r)

    assert(xs.length === 2)
  }
}
