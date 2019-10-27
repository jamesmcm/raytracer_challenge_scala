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

class Material(val colour: Colour, val ambient: Double, val diffuse: Double,
               val specular: Double, val shininess: Double) {

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Material => (colour === that.colour) && doubleEq(ambient, that.ambient) &&
        doubleEq(diffuse, that.diffuse) && doubleEq(specular, that.specular) && doubleEq(shininess, that.shininess)
      case _ => false
    }
  }

  final override def hashCode: Int = (colour, ambient, diffuse, specular, shininess).##

  def setColour(c: Colour): Material = new Material(c, ambient, diffuse, specular, shininess)
  def setAmbient(x: Double): Material = new Material(colour, x, diffuse, specular, shininess)
  def setDiffuse(x: Double): Material = new Material(colour, ambient, x, specular, shininess)
  def setSpecular(x: Double): Material = new Material(colour, ambient, diffuse, x, shininess)
  def setShininess(x: Double): Material = new Material(colour, ambient, diffuse, specular, x)

  def ambientContribution(effective_colour: Colour): Colour = {
    effective_colour * ambient
  }

  def diffuseContribution(effective_colour: Colour, light_dot_normal: Double): Colour = {
    if (light_dot_normal < 0) Colour.black else effective_colour * diffuse * light_dot_normal
  }

  def specularContribution(light_dot_normal: Double, reflect_dot_eye: Double, light_intensity: Colour): Colour = {
    if (light_dot_normal < 0 || reflect_dot_eye <= 0) Colour.black else light_intensity * specular * math.pow(reflect_dot_eye, shininess)
  }

  def lighting(light: Light, p: RTTuple, eyev: RTTuple, normalv: RTTuple): Colour = {
    val effective_colour: Colour = colour ** light.intensity // Colour blend
    val lightv: RTTuple = (light.position - p).normalise()
    val reflectv: RTTuple = lightv.negate().reflect(normalv)

    ambientContribution(effective_colour) +
      diffuseContribution(effective_colour, lightv.dot(normalv)) +
      specularContribution(lightv.dot(normalv), reflectv.dot(eyev), light.intensity)

  }
}

object Material{
  def defaultMaterial(): Material = new Material(Colour(1,1,1), 0.1, 0.9, 0.9, 200.0)
}