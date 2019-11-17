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

class IntersectionTest extends FunSuite {
  test("intersection.test_creation") {
    val s: Sphere = Sphere.unitSphere()
    val i: Intersection = new Intersection(3.5, s)

    assert(i.t === 3.5 && i.shape === s)
  }
  test("intersection.test_collection") {
    val s: Sphere = Sphere.unitSphere()
    val i1: Intersection = new Intersection(1, s)
    val i2: Intersection = new Intersection(2, s)
    val xs: Seq[Intersection] = Intersection.intersections(i1, i2)

    assert(xs.length === 2 && xs(0).t === 1 && xs(1).t === 2)
  }
  test("intersection.test_hit1") {
    val s: Sphere = Sphere.unitSphere()
    val i1: Intersection = new Intersection(1, s)
    val i2: Intersection = new Intersection(2, s)
    val xs: Seq[Intersection] = Intersection.intersections(i2, i1)

    assert(Intersection.hit(xs) === i1)
  }
  test("intersection.test_hit2") {
    val s: Sphere = Sphere.unitSphere()
    val i1: Intersection = new Intersection(-1, s)
    val i2: Intersection = new Intersection(1, s)
    val xs: Seq[Intersection] = Intersection.intersections(i2, i1)

    assert(Intersection.hit(xs) === i2)
  }
  test("intersection.test_hit3") {
    val s: Sphere = Sphere.unitSphere()
    val i1: Intersection = new Intersection(5, s)
    val i2: Intersection = new Intersection(7, s)
    val i3: Intersection = new Intersection(-3, s)
    val i4: Intersection = new Intersection(2, s)
    val xs: Seq[Intersection] = Intersection.intersections(i1, i2, i3, i4)

    assert(Intersection.hit(xs) === i4)
  }
  test("intersection.test_hit_shadow") {
    val s: Sphere = Sphere.unitSphere().setTransform(Translation(0, 0, 1))
    val r: Ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val i1: Intersection = new Intersection(5, s)
    val comps: Computation = Computation.prepareComputations(i1, r, Intersection.intersections(i1))

    assert(comps.over_point.z < -EPSILON / 2 && comps.point.z > comps.over_point.z)
  }

  test("intersection.test_reflection_vector") {
    val s: Plane = Plane()
    val r: Ray = Ray(Point(0, 1, -1), Vector(0, -1*math.sqrt(2)/2, math.sqrt(2)/2))
    val i1: Intersection = new Intersection(math.sqrt(2), s)
    val comps: Computation = Computation.prepareComputations(i1, r, Intersection.intersections(i1))

    assert(comps.reflectv === Vector(0, math.sqrt(2)/2, math.sqrt(2)/2))
  }
  test("intersection.test_glass_sphere0") {
    val a: Sphere = Sphere.glassSphere().setTransform(Scaling(2,2,2)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(1.5))
    val b: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, -0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.0))
    val c: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, 0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.5))
    val r: Ray = Ray(Point(0, 0, -4), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(Intersection(2, a), Intersection(2.75, b),
      Intersection(3.25, c), Intersection(4.75, b), Intersection(5.25, c), Intersection(6, a))
    val comps: Computation = Computation.prepareComputations(xs(0), r, xs)

    assert(comps.n1 === 1.0 && comps.n2 === 1.5)
  }
  test("intersection.test_glass_sphere1") {
    val a: Sphere = Sphere.glassSphere().setTransform(Scaling(2,2,2)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(1.5))
    val b: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, -0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.0))
    val c: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, 0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.5))
    val r: Ray = Ray(Point(0, 0, -4), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(Intersection(2, a), Intersection(2.75, b),
      Intersection(3.25, c), Intersection(4.75, b), Intersection(5.25, c), Intersection(6, a))
    val comps: Computation = Computation.prepareComputations(xs(1), r, xs)

    assert(comps.n1 === 1.5 && comps.n2 === 2.0)
  }
  test("intersection.test_glass_sphere2") {
    val a: Sphere = Sphere.glassSphere().setTransform(Scaling(2,2,2)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(1.5))
    val b: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, -0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.0))
    val c: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, 0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.5))
    val r: Ray = Ray(Point(0, 0, -4), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(Intersection(2, a), Intersection(2.75, b),
      Intersection(3.25, c), Intersection(4.75, b), Intersection(5.25, c), Intersection(6, a))
    val comps: Computation = Computation.prepareComputations(xs(2), r, xs)

    assert(comps.n1 === 2.0 && comps.n2 === 2.5)
  }
  test("intersection.test_glass_sphere3") {
    val a: Sphere = Sphere.glassSphere().setTransform(Scaling(2,2,2)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(1.5))
    val b: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, -0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.0))
    val c: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, 0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.5))
    val r: Ray = Ray(Point(0, 0, -4), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(Intersection(2, a), Intersection(2.75, b),
      Intersection(3.25, c), Intersection(4.75, b), Intersection(5.25, c), Intersection(6, a))
    val comps: Computation = Computation.prepareComputations(xs(3), r, xs)

    assert(comps.n1 === 2.5 && comps.n2 === 2.5)
  }
  test("intersection.test_glass_sphere4") {
    val a: Sphere = Sphere.glassSphere().setTransform(Scaling(2,2,2)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(1.5))
    val b: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, -0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.0))
    val c: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, 0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.5))
    val r: Ray = Ray(Point(0, 0, -4), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(Intersection(2, a), Intersection(2.75, b),
      Intersection(3.25, c), Intersection(4.75, b), Intersection(5.25, c), Intersection(6, a))
    val comps: Computation = Computation.prepareComputations(xs(4), r, xs)

    assert(comps.n1 === 2.5 && comps.n2 === 1.5)
  }
  test("intersection.test_glass_sphere5") {
    val a: Sphere = Sphere.glassSphere().setTransform(Scaling(2,2,2)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(1.5))
    val b: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, -0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.0))
    val c: Sphere = Sphere.glassSphere().setTransform(Translation(0, 0, 0.25)).setMaterial(Sphere.glassSphere().material.setRefractiveIndex(2.5))
    val r: Ray = Ray(Point(0, 0, -4), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(Intersection(2, a), Intersection(2.75, b),
      Intersection(3.25, c), Intersection(4.75, b), Intersection(5.25, c), Intersection(6, a))
    val comps: Computation = Computation.prepareComputations(xs(5), r, xs)

    assert(comps.n1 === 1.5 && comps.n2 === 1.0)
  }
  test("intersection.test_under_point") {
    val a: Sphere = Sphere.glassSphere().setTransform(Translation(0,0,1))
    val r: Ray = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val i: Intersection = Intersection(5, a)
    val xs: Seq[Intersection] = Intersection.intersections(i)
    val comps: Computation = Computation.prepareComputations(i, r, xs)

    assert(comps.under_point.z > EPSILON/2 && comps.point.z < comps.under_point.z)
  }


}

