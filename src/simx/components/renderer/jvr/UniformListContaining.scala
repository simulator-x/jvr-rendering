/*
 * Copyright 2012 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.renderer.jvr

import simx.core.ontology.SVarDescription

/**
 * This is a common base class for the connection between shader uniform and SVarDescription.
 *
 * It is used by [[simx.components.renderer.jvr.PostProcessingEffect]] and [[simx.components.renderer.jvr.RenderPass]].
 *
 * @author Stephan Rehfeld
 *
 * @tparam T The type of the container. RenderPass oder PostProcessingEffect.
 */
abstract class UniformListContaining[T <: UniformListContaining[T] ] {

  /**
   * The uniforms of the shader.
   */
  var uniformList : List[UniformManager[_, _,T]]
}

/**
 * A helper class that helps to construct a [[simx.components.renderer.jvr.UniformManager]].
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the uniform.
 * @param parent The parent object. [[simx.components.renderer.jvr.PostProcessingEffect]] or [[simx.components.renderer.jvr.RenderPass]].
 * @tparam P [[simx.components.renderer.jvr.PostProcessingEffect]] or [[simx.components.renderer.jvr.RenderPass]].
 */
class UniformNameHolder[P <: UniformListContaining[P] ]( name: String, parent : P ) {
  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( name != "", "The parameter 'name' must not be an empty string!" )
  require( parent != null, "The parameter 'parent' must not be 'null'!" )

  /**
   * This method sets the value of the uniform.
   *
   * @param value The value of the uniform.
   * @tparam T The type of the uniform.
   * @return The final uniform manager.
   */
  def hasValue[T]( value : T ) = new UniformManager[T, T, P]( name, value, None, parent, (x => x, x => x) )
}

/**
 * A uniform manager connects a name of a uniform with a value, a type and an SVarDescription for the entity.
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the uniform.
 * @param value The value of the uniform.
 * @param ontologyMember The optional SVarDescription for the uniform.
 * @param parent The parent object. [[simx.components.renderer.jvr.PostProcessingEffect]] or [[simx.components.renderer.jvr.RenderPass]].
 * @tparam T The type of the uniform.
 * @tparam P [[simx.components.renderer.jvr.PostProcessingEffect]] or [[simx.components.renderer.jvr.RenderPass]].
 */
class UniformManager[T, U, P <: UniformListContaining[P] ]( val name: String, val value : T, var ontologyMember : Option[ SVarDescription[U, _] ], parent : P , val converter : (U => T, T => U)) extends Serializable {
  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( name != "", "The parameter 'name' must not be an empty string!" )
  require( parent != null, "The parameter 'parent' must not be 'null'!" )

  parent.uniformList = parent.uniformList ::: List[UniformManager[_, _, P]](this)

  def convertedBy[V](converter : (V => T, T => V)) : UniformManager[T, V, P] = {
    parent.uniformList = parent.uniformList.filterNot( _ == this )
    ontologyMember match {
      case Some(x : SVarDescription[V, _]) => new UniformManager(name, value, Some(x.asInstanceOf[SVarDescription[V, _]]), parent, converter)
      case None => new UniformManager(name, value, None, parent, converter)
      case _ => throw new Exception("cannot specify converter after specifying ontology member")
    }
  }

  /**
   * This method sets an sVar description for the uniform. The constructed entity will contain an sVar that manipulates
   * this uniform.
   *
   * @param om The SVarDescription for the uniform.
   * @return The altered uniform manager.
   */
  def isReachableBy( om : SVarDescription[U, _] ) : UniformManager[T, U, P] = {
    require( om != null, "The parameter 'om' must not be 'null'!" )
    ontologyMember = Some( om )
    this
  }

  /**
   * Adds another uniform.
   *
   * @param name The name of the uniform.
   * @return The uniform name holder to continue construction.
   */
  def and( name : String ) : UniformNameHolder[P] = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not be an empty string!" )
    new UniformNameHolder( name, parent )
  }

  /**
   * This function returns the parent element. [[simx.components.renderer.jvr.PostProcessingEffect]] or [[simx.components.renderer.jvr.RenderPass]].
   *
   * @return A [[simx.components.renderer.jvr.PostProcessingEffect]] or [[simx.components.renderer.jvr.RenderPass]].
   */
  def pack : P = parent
}