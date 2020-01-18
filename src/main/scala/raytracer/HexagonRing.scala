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

object HexagonRing {

  def hexagonCorner(m: Material): Sphere = {
    Sphere.unitSphere().setMaterial(m).setTransform(Translation(0,0,-1) * Scaling(0.25, 0.25, 0.25))
  }

  def hexagonEdge(m: Material): Cylinder = {
    Cylinder().setMinimum(0).setMaximum(1).setMaterial(m)
      .setTransform(
        Translation(0, 0, -1)*
    RotationY(-math.Pi / 6.0) *
    RotationZ(-math.Pi / 2.0) *
    Scaling(0.25, 1, 0.25)
    )
  }

  def hexagonSide(m: Material): Group = {
    Group().addChild(hexagonCorner(m)).addChild(hexagonEdge(m))
  }

  def hexagon(m: Material): Group = {
    val g: Group = Group()
    (0 to 5).foldLeft(g)((acc: Group, x: Int) => acc.addChild(hexagonSide(m).setTransform(RotationY(x*math.Pi / 3.0))))
  }
}
