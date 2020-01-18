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
  test("Group.test_world_to_object") {
    val s: Sphere = Sphere.unitSphere().setTransform(Translation(5, 0, 0))
    val g2: Group    = Group().setTransform(Scaling(2,2,2)).addChild(s)
    val g1: Group    = Group().setTransform(RotationY(math.Pi/2.0)).addChild(g2)

    val p: RTTuple = s.worldToObject(Point(-2, 0, -10))
    assert(p === Point(0, 0, -1))
  }
  test("Group.normal_object_to_world") {
    val s: Sphere = Sphere.unitSphere().setTransform(Translation(5, 0, 0))
    val g2: Group    = Group().setTransform(Scaling(1,2,3)).addChild(s)
    val g1: Group    = Group().setTransform(RotationY(math.Pi/2.0)).addChild(g2)

    val n: RTTuple = s.normalToWorld(Vector(math.sqrt(3)/3, math.sqrt(3)/3, math.sqrt(3)/3))
    assert(n === Vector(0.285714, 0.4285714, -0.857142))
  }
  test("Group.normal_at_child") {
    val s: Sphere = Sphere.unitSphere().setTransform(Translation(5, 0, 0))
    val g2: Group    = Group().setTransform(Scaling(1,2,3)).addChild(s)
    val g1: Group    = Group().setTransform(RotationY(math.Pi/2.0)).addChild(g2)

    val n: RTTuple = s.normalAt(Point(1.7321, 1.1547, -5.5774))
    assert(n === Vector(0.2857037, 0.428543, -0.85716))
  }
  test("Group.bounds_unit_sphere") {
    val s: Sphere = Sphere.unitSphere()
    val g2: Group    = Group().addChild(s).setBounds()

    assert(g2.stored_bounds === Some((Point(-1, -1, -1), Point(1, 1, 1))))
  }
  test("Group.bounds_unit_sphere_transform1") {
    val s: Sphere = Sphere.unitSphere().setTransform(Scaling(2,2,2))
    val g2: Group    = Group().addChild(s).setBounds()

    assert(g2.stored_bounds === Some((Point(-2, -2, -2), Point(2, 2, 2))))
  }
  test("Group.bounds_unit_sphere_group_transform1") {
    val s: Sphere = Sphere.unitSphere()
    val g2: Group    = Group().addChild(s).setTransform(Scaling(2,2,2)).setBounds()

    assert(g2.stored_bounds === Some((Point(-1, -1, -1), Point(1, 1, 1))))
  }
  test("Group.bounds_unit_cube") {
    val s: Cube = Cube()
    val g2: Group    = Group().addChild(s).setBounds()

    assert(g2.stored_bounds === Some((Point(-1, -1, -1), Point(1, 1, 1))))
  }
  test("Group.bounds_unit_cube_transform") {
    val s: Cube = Cube().setTransform(Scaling(5, 1, 1))
    val g2: Group    = Group().addChild(s).setBounds()

    assert(g2.stored_bounds === Some((Point(-5, -1, -1), Point(5, 1, 1))))
  }
  test("Group.bounds_unit_cube_transform_rotate") {
    val s: Cube = Cube().setTransform(RotationZ(math.Pi/2.0) * Scaling(5, 1, 1))
    val g2: Group    = Group().addChild(s).setBounds()

    assert(g2.stored_bounds === Some((Point(-1, -5, -1), Point(1, 5, 1))))
  }
  test("Group.bounds_unit_cube_transform_rotate_group") {
    val s: Cube = Cube()
    val g2: Group    = Group().addChild(s).setTransform(RotationZ(math.Pi/2.0) * Scaling(5, 1, 1)).setBounds()

    assert(g2.stored_bounds === Some((Point(-1, -1, -1), Point(1, 1, 1))))
  }
  test("Group.bounds_unit_sphere_translate") {
    val s: Sphere = Sphere.unitSphere()
    val s2: Sphere = Sphere.unitSphere().setTransform(Translation(5,0,0))
    val g2: Group    = Group().addChild(s).addChild(s2).setBounds()

    assert(g2.stored_bounds === Some((Point(-1, -1, -1), Point(6, 1, 1))))
  }
}
