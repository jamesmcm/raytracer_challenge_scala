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

class ObjParserTest extends FunSuite {
  test("ObjParser.test_nonsense") {
    val p: ObjParser = new ObjParser
    assert(p.parse("obj/badtest.obj")  === 5)
  }

  test("ObjParser.test_vertices") {
    val p: ObjParser = new ObjParser
    assert(p.parse("obj/vertices_test.obj")  === 0 &&
      p.vertices(0) === Point(-1,1,0) &&
      p.vertices(1) === Point(-1,0.5,0) &&
      p.vertices(2) === Point(1,0,0) &&
      p.vertices(3) === Point(1,1,0)
    )
  }
  test("ObjParser.test_faces_triangles") {
    val p: ObjParser = new ObjParser
    val ignored_lines: Int = p.parse("obj/faces_triangle_test.obj")
    val t1: Triangle = p.defaultGroup.objs(0).asInstanceOf[Triangle]
    val t2: Triangle = p.defaultGroup.objs(1).asInstanceOf[Triangle]

    assert(ignored_lines === 1 &&
      p.vertices(0) === t1.points._1 &&
      p.vertices(1) === t1.points._2 &&
      p.vertices(2) === t1.points._3 &&
      p.vertices(0) === t2.points._1 &&
      p.vertices(2) === t2.points._2 &&
      p.vertices(3) === t2.points._3
    )
  }
  test("ObjParser.test_faces_triangulation") {
    val p: ObjParser = new ObjParser
    val ignored_lines: Int = p.parse("obj/triangulation_test.obj")
    val t1: Triangle = p.defaultGroup.objs(0).asInstanceOf[Triangle]
    val t2: Triangle = p.defaultGroup.objs(1).asInstanceOf[Triangle]
    val t3: Triangle = p.defaultGroup.objs(2).asInstanceOf[Triangle]

    assert(ignored_lines === 1 &&
      p.vertices(0) === t1.points._1 &&
      p.vertices(1) === t1.points._2 &&
      p.vertices(2) === t1.points._3 &&
      p.vertices(0) === t2.points._1 &&
      p.vertices(2) === t2.points._2 &&
      p.vertices(3) === t2.points._3 &&
      p.vertices(0) === t3.points._1 &&
      p.vertices(3) === t3.points._2 &&
      p.vertices(4) === t3.points._3
    )
  }
  test("ObjParser.test_groups") {
    val p: ObjParser = new ObjParser
    val ignored_lines: Int = p.parse("obj/groups_test.obj")
    val t1: Triangle = p.groups("FirstGroup").objs(0).asInstanceOf[Triangle]
    val t2: Triangle = p.groups("SecondGroup").objs(0).asInstanceOf[Triangle]

    assert(ignored_lines === 1 &&
      p.vertices(0) === t1.points._1 &&
      p.vertices(1) === t1.points._2 &&
      p.vertices(2) === t1.points._3 &&
      p.vertices(0) === t2.points._1 &&
      p.vertices(2) === t2.points._2 &&
      p.vertices(3) === t2.points._3
    )
  }
  test("ObjParser.test_groups_output") {
    val p: ObjParser = new ObjParser
    val ignored_lines: Int = p.parse("obj/groups_test.obj")
    val g: Group = p.toGroup()

    assert(ignored_lines === 1 &&
      p.vertices(0) === (g.objs(0)).asInstanceOf[Group].objs(0).asInstanceOf[Triangle].points._1 &&
      p.vertices(1) === (g.objs(0)).asInstanceOf[Group].objs(0).asInstanceOf[Triangle].points._2 &&
      p.vertices(2) === (g.objs(0)).asInstanceOf[Group].objs(0).asInstanceOf[Triangle].points._3 &&
      p.vertices(0) === (g.objs(1)).asInstanceOf[Group].objs(0).asInstanceOf[Triangle].points._1 &&
      p.vertices(2) === (g.objs(1)).asInstanceOf[Group].objs(0).asInstanceOf[Triangle].points._2 &&
      p.vertices(3) === (g.objs(1)).asInstanceOf[Group].objs(0).asInstanceOf[Triangle].points._3
    )
  }
}
