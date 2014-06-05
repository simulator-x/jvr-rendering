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
import java.io.{ObjectInputStream, ObjectOutputStream, File}
import de.bht.jvr.core.{ShaderProgram, ShaderMaterial}
import de.bht.jvr.math.Vector2
import de.bht.jvr.core.uniforms._
import simx.core.entity.typeconversion.ConvertibleTrait
import de.bht.jvr.util.Color
import simx.core.entity.Entity
import simx.core.svaractor.{SVarActor, SVar}
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import scala.annotation.meta.field


/**
 * A domain specific language to specify shader effects for jVR and make parameters of them accessible to the trough
 * the middle ware.
 *
 * Different shader can be used for every render pass. The following example describes a parallax mapping.
 *
 * {{{
  def makeParallaxMaterial( name : String, file : String, heightScale: Float, heightBias: Float, shininess : Float ) =
    ShaderEffect( name ) has(
      RenderPass( "AMBIENT" ) describedByShader( "shader/bumpmapping_ambient.vs" :: "shader/parallaxmapping_ambient.fs" :: Nil ) provideImage
        (file + "_COLOR.jpg") as "jvr_Texture0" provideImage
        (file + "_DISP.jpg") as "jvr_HeightMap" where
        "jvr_HeightScale" hasValue( heightScale ) and
        "jvr_ParallaxBias" hasValue( heightBias ) and
        "jvr_Material_Ambient" hasValue( new Color( 0.1f, 0.1f, 0.1f, 1f ) ) pack
      ) and( RenderPass( "LIGHTING" ) describedByShader( "shader/bumpmapping_lighting.vs" :: "shader/parallaxmapping_lighting.fs" :: Nil ) provideImage
      (file + "_COLOR.jpg") as "jvr_Texture0" provideImage
      (file + "_DISP.jpg") as "jvr_HeightMap" provideImage
      (file + "_NORMAL.jpg") as "jvr_NormalMap" where
      "jvr_HeightScale" hasValue( heightScale ) and
      "jvr_ParallaxBias" hasValue( heightBias ) and
      "jvr_Material_Diffuse" hasValue( new Color(1.0f, 1.0f, 1.0f, 1.0f ) ) and
      "jvr_Material_Specular" hasValue( new Color(0.6f, 0.6f, 0.6f, 1.0f) ) and
      "jvr_Material_Shininess" hasValue( shininess ) pack
      )

 * }}}
 *
 * @param name The name of the shader effect.
 */
class ShaderEffect( name : String ) extends Serializable {

  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( name != "", "The parameter 'name' must not be an empty string!" )

  /**
   * The list of all render passes.
   */
  private var renderPasses = List[RenderPass]()

  /**
   * The constructed shader material for this effect.
   */
  @(transient @field) private var shaderMaterial : Option[ShaderMaterial] = None

  /**
   * This method adds a render pass to this shader effect.
   *
   * @param renderPass The render pass to add.
   * @return The altered shader effect.
   */
  def has( renderPass : RenderPass ) : ShaderEffect = {
    require( renderPass != null, "The parameter 'renderPass' must not be 'null'!" )
    renderPasses = renderPasses ::: renderPass :: Nil
    this
  }

  /**
   * This method adds a render pass to this shader effect.
   *
   * @param renderPass The render pass to add.
   * @return The altered shader effect.
   */
  def and( renderPass : RenderPass ) : ShaderEffect = has( renderPass )

  /**
   * This method constructs and returns the shader material for this effect.
   *
   * @return The shader material for this effect.
   */
  private[jvr] def getShaderMaterial : ShaderMaterial = {
    if( !shaderMaterial.isDefined ) {
      val sm = new ShaderMaterial( )
      for( renderPass <- renderPasses ) {
        sm.setShaderProgram( renderPass.name, renderPass.getShaderProgram )
        for( (image,name) <- renderPass.images )
          sm.setTexture( renderPass.name, name, ResourceManager.loadTexture( new File( image ) ) )
        for( uniform <- renderPass.uniformList ) {
          val uniformValue = uniform.value match {
            case v : Float =>   new UniformFloat( v )
            case v : Int =>     new UniformInt( v )
            case v : Boolean => new UniformBool( v )
            case v : Vector2 => new UniformVector2( v )
            case v : Color =>   new UniformColor( v )
            case v : Seq[_] =>
              sm.setUniform( renderPass.name, uniform.name + "_size", new UniformInt( v.size ) )
              v.head match {
                case h : Vector2 =>
                  new UniformVector2( v.asInstanceOf[Seq[Vector2]].toArray : _* )
              }
          }
          sm.setUniform( renderPass.name, uniform.name, uniformValue )
        }
      }

      shaderMaterial = Some( sm )
    }
    shaderMaterial.get
  }

  private[jvr] def setShaderMaterial(sm : ShaderMaterial) {
    shaderMaterial = Some(sm)
  }

  def bindShaderMaterialToEntity( entity : Entity )(implicit actorContext : EntityUpdateHandling) = {
    for( renderPass <- renderPasses ) {
      for( uniformManager <- renderPass.uniformList ) {
        if( uniformManager.ontologyMember.isDefined )
          uniformManager.value match {
            case v : Float =>
              val material = getShaderMaterial
              entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Float]].observe{
                v =>material.setUniform( renderPass.name, uniformManager.name, new UniformFloat( v ) )
              }
            case v : Int =>
              entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Int]].observe{
                v =>  getShaderMaterial.setUniform( renderPass.name, uniformManager.name, new UniformInt( v ) )
              }
            case v : Boolean =>
              entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Boolean]].observe{
                v => getShaderMaterial.setUniform( renderPass.name, uniformManager.name, new UniformBool( v ) )
              }
            case v : Vector2 =>
              entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Vector2]].observe{
                v => getShaderMaterial.setUniform( renderPass.name, uniformManager.name, new UniformVector2( v ) )
              }
            case v : Seq[_] =>
              v.head match {
                case h : Vector2 =>
                  entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Seq[Vector2]]].observe{ v : Seq[Vector2] =>
                    getShaderMaterial.setUniform( renderPass.name, uniformManager.name, new UniformVector2( v.toArray : _* ) )
                    getShaderMaterial.setUniform( renderPass.name, uniformManager.name + "_size", new UniformInt( v.size ) )
                  }
              }

          }
      }
    }
  }

  /**
   * This method returns a list of all sVar Descriptions that are provided by this effect. It is used during the
   * construction process of the entity.
   *
   * @return A list of all sVar descriptions provided by this effect.
   */
  private[jvr] def getSVarDescriptions : List[SVarDescription[_,_]] = {
    var sVarDescriptions = List[SVarDescription[_,_]]()
    for( renderPass <- this.renderPasses ) {
      for( uniformManager <- renderPass.uniformList ) {
        if( uniformManager.ontologyMember.isDefined ) {
          sVarDescriptions = uniformManager.ontologyMember.get :: sVarDescriptions
        }
      }
    }
    sVarDescriptions
  }

  /**
   * This method return the initial value for an sVar.
   *
   * @param sVarDescription The description of the sVar
   * @tparam T The type of the variable.
   * @return The initial value of the sVar.
   */
  private[jvr] def getValueForSVarDescription[T]( sVarDescription : ConvertibleTrait[T]) : T = {
    var value : Option[T] = None
    for( renderPass <- this.renderPasses ) {
      for( uniformManager <- renderPass.uniformList ) {
        if( uniformManager.ontologyMember.isDefined && uniformManager.ontologyMember.get == sVarDescription ) {
          value = Some( uniformManager.value.asInstanceOf[T] )
        }
      }
    }
    value.get

  }

  private def writeObject( objectOutputStream : ObjectOutputStream ) {
    objectOutputStream.defaultWriteObject()
  }

  private def readObject( objectInputStream : ObjectInputStream ) {
    objectInputStream.defaultReadObject()
    shaderMaterial = None
  }

}

/**
 * An object that configure the shader and uniforms for one render pass. Typical render passes are AMBIENT and LIGHTING.
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the render pass.
 */
class RenderPass( val name : String ) extends UniformListContaining[RenderPass] with Serializable {

  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( name != "", "The parameter 'name' must not be an empty string!" )

  /**
   * The list of shader that are used by this effect.
   */
  private var shader = List[String]()

  /**
   * Textures and the names under which they are provided to the shader.
   */
  private[jvr] var images : Map[String,String] = Map()

  /**
   * The shader material that is constructed for this post processing effect.
   */
  @(transient @field) private var shaderProgram : Option[ShaderProgram] = None

  override var uniformList : List[UniformManager[_, _,RenderPass]] = List()

  /**
   * This method sets the shader that are used to render this effect.
   *
   * @param shader The list of shader, that are used to render this effect.
   * @return The altered shader effect.
   */
  def describedByShader( shader : List[String] ) : RenderPass  = {
    require( shader != null, "The parameter 'shader' must not be 'null'!" )
    require( shader.size > 1, "The parameter 'shader' contain at least 2 entries!" )
    this.shader = shader
    this
  }

  /**
   * This method adds a texture texture to the shader.
   *
   * @param file The file name of the texture.
   * @return The object to construct the texture description.
   */
  def provideImage( file : String ) : TextureMapper = {
    require( file != null, "The parameter 'name' must not be 'null'!" )
    require( file != "", "The parameter 'name' must not an empty string'!" )
    new TextureMapper( file, this )
  }

  /**
   * This is a small helper class for the shader effect domain specific language.
   *
   * @author Stephan Rehfeld
   *
   * @param file The file name of the texture
   * @param renderPass The render pass.
   */
  class TextureMapper( val file : String, val renderPass : RenderPass ) {

    require( file != null, "The parameter 'file' must not be 'null'!" )
    require( file != "", "The parameter 'file' must not an empty string'!" )
    require( renderPass != null, "The parameter 'renderPass' must not be 'null'!" )

    /**
     * This method sets the name under which the texture should be provided to the render pass.
     *
     * @param targetName The name under which the texture should be provided to the render pass.
     * @return The altered post processing effect.
     */
    def as( targetName : String ) : RenderPass = {
      require( targetName != null, "The parameter 'targetName' must not be 'null'!" )
      require( targetName != "", "The parameter 'targetName' must not an empty string'!" )
      images = images + (file -> targetName )
      renderPass
    }

  }

  /**
   * This method starts to add a uniform to the render pass.
   *
   * @param name The name of the uniform.
   * @return The object to construct the uniform description.
   */
  def where( name : String ) = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not an empty string'!" )
    new UniformNameHolder( name, this )
  }

  /**
   * This method constructs and returns the shader material for this render pass.
   *
   * @return The shader material for this render pass.
   */
  def getShaderProgram : ShaderProgram = {
    if( shaderProgram.isEmpty ) {
      val shaderFiles = for( file <- shader ) yield new File( file )
      val sp = new ShaderProgram( shaderFiles: _* )
      shaderProgram = Some( sp )
    }
    this.shaderProgram.get
  }

  private def writeObject( objectOutputStream : ObjectOutputStream ) {
    objectOutputStream.defaultWriteObject()
  }

  private def readObject( objectInputStream : ObjectInputStream ) {
    objectInputStream.defaultReadObject()
    shaderProgram = None
  }

}

/**
 * The companion object of the render pass.
 *
 * @see [[simx.components.renderer.jvr.RenderPass]]
 *
 * @author Stephan Rehfeld
 */
object RenderPass {

  /**
   * The method to start a description of a render pass.
   *
   * @param name The name of the render pass.
   * @return The render pass.
   */
  def apply( name : String ) : RenderPass = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not be an empty string!" )
    new RenderPass( name )
  }
}

/**
 * The companion object of the shader effect.
 *
 * @see [[simx.components.renderer.jvr.ShaderEffect]]
 *
 * @author Stephan Rehfeld
 */
object ShaderEffect {

  /**
   * The method to start a description of a shader effect.
   *
   * @param name The name of the shader effect.
   * @return The shader effect.
   */
  def apply( name : String ) : ShaderEffect = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    new ShaderEffect( name )
  }

}