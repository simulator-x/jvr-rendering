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

import de.bht.jvr.core.pipeline.Pipeline
import de.bht.jvr.core.{Texture2D, ShaderProgram, ShaderMaterial}
import java.io.{ObjectInputStream, ObjectOutputStream, File}
import simx.core.ontology.SVarDescription
import de.bht.jvr.util.Color
import simx.core.entity.typeconversion.ConvertibleTrait
import de.bht.jvr.core.uniforms._
import de.bht.jvr.math.{Vector4, Vector2}
import simplex3d.math.floatx.{ConstVec4f, Vec2f}
import simx.core.helper.TextureData
import simx.core.entity.description.SVal

/**
 * This class implements a domain specific language to configure post processing effects that are renderer by jVR.
 *
 * The post processing effects are executed after the geometry of the scene has been renderer. They are getting that
 * color frame buffer and the depth buffer as input. Typical post processing effect are interfaces, depth of field, and
 * fog.
 *
 * Examples:
 *
 * Fog
 * {{{
   val fogPpe = PostProcessingEffect( "fog" ).
      describedByShaders( "pipeline_shader/quad.vs" :: "pipeline_shader/fog.fs" :: Nil ).
      usingColorBufferAsName( "jvr_Texture1" ).usingDepthBufferAsName( "jvr_Texture0" ).
      where( "nearClip" ).hasValue( nearClip ).isReachableBy( oTypes.NearClip ).
      and( "farClip" ).hasValue( farClip ).isReachableBy( oTypes.FarClip ).
      and( "skyFog" ).hasValue( skyBoxFog ).isReachableBy( oTypes.SkyBoxFog ).pack
 * }}}
 *
 * Depth of field
 * {{{
   val depthOfFieldPpe = PostProcessingEffect( "dof" ).
     describedByShaders( "pipeline_shader/quad.vs" :: "pipeline_shader/dof.fs" :: Nil ).
     usingColorBufferAsName( "jvr_Texture1" ).usingDepthBufferAsName( "jvr_Texture0" ).
     where( "intensity" ).hasValue( fresh.firstValueFor( oTypes.Intensity ) ).isReachableBy( oTypes.Intensity ).pack
 * }}}
 *
 * Interface
 * {{{
   val interfacePpe = PostProcessingEffect( "interface" ).
     describedByShaders( "pipeline_shader/quad.vs" :: "shader/interface.fs" :: Nil ).
     isAnOverlay.
     where( "lives" ).hasValue( 3 ).isReachableBy( oTypes.Lives ).
     and( "health").hasValue( 1.0f ).isReachableBy( oTypes.Health ).
     and( "mana" ).hasValue( 1.0f ).isReachableBy( oTypes.Mana ).pack.
     provideImage( "simthief/images/heart.png" ).as( "iface_live" )
 * }}}
 *
 * @author Stephan Rehfeld
 */
class PostProcessingEffect() extends UniformListContaining[PostProcessingEffect] with Serializable {

  /**
   * The name of this effect.
   */
  private[jvr] var nameOfEffect : Option[String] = None

  /**
   * The name of the target frame buffer object. This is useful if the effect is realized by more than one step.
   */
  private var targetFBO : Option[String] = None

  /**
   * The size ratio for the target frame buffer object. 1.0f means the same size as the frame buffer. Useful for
   * downsampling.
   */
  private var ratio : Option[Float] = None

  /**
   * Should the target frame buffer object be cleared before rendering?
   */
  private var clearTarget : Boolean = true

  /**
   * The list of shader that are used by this post processing effect.
   */
  private var shader : List[String] = List()

  /**
   * The color buffer is provided by this name to the shader.
   */
  private var nameForColorBuffer : Option[String] = None

  /**
   * The depth buffer is provided by this name to the shader.
   */
  private var nameForDepthBuffer : Option[String] = None

  /**
   * An internal map that saves which frame buffer object is provided under which name ot the shader.
   */
  private var usedFrameBufferObjects : Map[String,String] = Map()

  /**
   * The shader material that is constructed for this post processing effect.
   */
  @transient private var shaderMaterial : Option[ShaderMaterial] = None

  /**
   * The name under which the time since the last frame is provided to the shader.
   */
  private var deltaTName : Option[String] = None

  override var uniformList : List[UniformManager[_, _, PostProcessingEffect]] = List()

  /**
   * Textures and the names under which they are provided to the shader.
   */
  private[jvr] var images : Map[String,Texture2D] = Map()

  /**
   * Indicates if the effect is an overlay like an interface. The result is not written to a new frame buffer object,
   * but the effect is drawn in the current fbo.
   */
  private[jvr] var isOverlay = false

  /**
   * Indicates if the target fbo should be created or if it already exists from a previous post processing effect.
   */
  private[jvr] var createTargetFBO = true

  /**
   * Sets the name of the uniform under which the time in seconds since the last renderer frame is provided to the
   * shader.
   *
   * @param deltaTName The name of the uniform.
   * @return The altered post processing effect.
   */
  def provideDeltaTAs( deltaTName : String ) = {
    require( deltaTName != null, "The parameter 'deltaTName' must not be 'null'!" )
    require( deltaTName != "", "The parameter 'deltaTName' must not an empty string'!" )
    this.deltaTName = Some( deltaTName )
    this
  }

  /**
   * This method sets if the post processing effect is an overlay like an interface.
   *
   * @return The altered post processing effect.
   */
  def isAnOverlay : PostProcessingEffect = {
    this.isOverlay = true
    this
  }

  /**
   * This method sets the shader that are used to render this post processing effect.
   *
   * @param shader The list of shader, that are used to render this effect.
   * @return The altered post processing effect.
   */
  def describedByShaders( shader : List[String] ) : PostProcessingEffect  = {
    require( shader != null, "The parameter 'shader' must not be 'null'!" )
    require( shader.size > 1, "The parameter 'shader' contain at least 2 entries!" )
    this.shader = shader
    this
  }

  /**
   * This method sets an optional name for the frame buffer object this effects write it's result into. This is useful
   * for effects that are described by more than one rendering step. The target frame buffer object is cleared before
   * the rendering begins.
   *
   * @param name The name of the target frame buffer object.
   * @return The altered post processing effect.
   */
  def writesResultIn( name : String ) : PostProcessingEffect = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not an empty string'!" )
    targetFBO = Some( name )
    createTargetFBO = true
    this
  }

  /**
   * This method sets an optional name for the frame buffer object this effects write it's result into. This is useful
   * for effects that are described by more than one rendering step. The target frame buffer object is not cleared
   * before the rendering begins.
   *
   * @param name The name of the target frame buffer object.
   * @return The altered post processing effect.
   */
  def writesResultInAlreadyCreated( name : String ) : PostProcessingEffect = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not an empty string'!" )
    targetFBO = Some( name )
    createTargetFBO = false
    this
  }

  /**
   * This method manipulates if the target frame buffer object should be cleared before rendering.
   *
   * @param clear Pass 'true' if the target frame buffer object should be cleared before rendering.
   * @return The altered post processing effect.
   */
  def clearTarget( clear : Boolean ) : PostProcessingEffect = {
    clearTarget = clear
    this
  }

  /**
   * This method sets the size of the target frame buffer object in relation to the frame buffer.
   *
   * @param ratio The size of the target fbo in relation to the frame buffer. A value larger than 0.0.
   * @return The altered post processing effect.
   */
  def withRatio( ratio : Float )  : PostProcessingEffect = {
    require( ratio > 0.0f, "The parameter 'ratio' must be larger than 0.0!")
    this.ratio = Some( ratio )
    this
  }

  /**
   * The name under which the color buffer should be provided to the shader as a texture.
   *
   * @param name The name under which the color buffer should be provided to the shader as a texture.
   * @return The altered post processing effect.
   */
  def usingColorBufferAsName( name : String ) : PostProcessingEffect = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not an empty string'!" )
    nameForColorBuffer = Some( name )
    this
  }

  /**
   * The name under which the depth buffer should be provided to the shader as a texture.
   *
   * @param name The name under which the depth buffer should be provided to the shader as a texture.
   * @return The altered post processing effect.
   */
  def usingDepthBufferAsName( name : String )  : PostProcessingEffect = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not an empty string'!" )
    nameForDepthBuffer = Some( name )
    this
  }

  /**
   * The method adds a target frame buffer of a previous post processing effect as a texture to the current post
   * processing effect.
   *
   * @param name The name of the target fbo of a previous post processing effect.
   * @return A frame buffer mapper for the next command.
   */
  def usingFrameBufferObjectWithName( name : String ) :  FrameBufferMapper = {
    require( name != null, "The parameter 'name' must not be 'null'!" )
    require( name != "", "The parameter 'name' must not an empty string'!" )
    new FrameBufferMapper( name, this )
  }

  /**
   * This method starts to add a uniform to the post processing effect.
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
   * This method adds a texture texture to the shader.
   *
   * @param tex The texture.
   * @return The object to construct the texture description.
   */
  def provideImage( tex : Texture2D ) : TextureMapper = {
    require( tex != null, "The parameter 'name' must not be 'null'!" )
    new TextureMapper( tex, this )
  }



  /**
   * This method constructs and returns the shader material for this effect.
   *
   * @return The shader material for this effect.
   */
  private[jvr] def getShaderMaterial : ShaderMaterial = {
    if( !shaderMaterial.isDefined ) {
      val shaderFiles = for( file <- shader ) yield new File( file )
      val sm = new ShaderMaterial( nameOfEffect.get, new ShaderProgram( shaderFiles: _* ) )
      for( uniform <- uniformList ) {
        uniform.value match {
          case v : Texture2D =>
            sm.setTexture( nameOfEffect.get, uniform.name, v )
          case otherValue => val uniformValue = otherValue match {
            case v : Float =>
              new UniformFloat( v )
            case v : Int =>
              new UniformInt( v )
            case v : Boolean =>
              new UniformBool( v )
            case v : Vector2 =>
              new UniformVector2( v )
            case v : ConstVec4f =>
              new UniformVector4(new Vector4(v.x, v.y, v.z, v.w))
            case v : List[_] =>
              sm.setUniform( nameOfEffect.get, uniform.name + "_size", new UniformInt( v.size ) )
              if( v.isEmpty ) {
                new UniformVector2( new Vector2( 0.0f, 0.0f ) )
              } else {
                v.head match {
                  case h : Vec2f => {
                    val jvrList = v.asInstanceOf[List[Vec2f]].map(s3dVec => {new Vector2(s3dVec.x, s3dVec.y)})
                    new UniformVector2( jvrList.toSeq.toArray : _* )
                  }
                }
              }
          }
            sm.setUniform( nameOfEffect.get, uniform.name, uniformValue )
        }
      }
      shaderMaterial = Some( sm )
    }
    shaderMaterial.get
  }

  /**
   * This method adds the necessary rendering commands to the pipeline.
   *
   * @param pipeline The pipeline where the commands should be added to.
   * @param colorFBO The name of the color buffer which should be provided to the shader.
   * @param depthFBO The name of the depth buffer which should be provided to the shader.
   * @return Returns true if the results are written to the previous set color buffer. Returns false if this effect has a target fbo or is an overlay.
   */
  private[jvr] def contributeToPipeline( pipeline : Pipeline, colorFBO : String, depthFBO : String ) : Boolean = {
    var switchFBO = true
    if( targetFBO.isDefined ) {
      if( createTargetFBO) pipeline.createFrameBufferObject( targetFBO.get, true, 1, this.ratio.getOrElse( 1.0f ), 4 )
      pipeline.switchFrameBufferObject( targetFBO.get )
      if( this.clearTarget ) pipeline.clearBuffers( true, true, new Color(0,0,0) )
      switchFBO = false
    }
    if( nameForColorBuffer.isDefined ) pipeline.bindColorBuffer( nameForColorBuffer.get, colorFBO, 0 )
    if( nameForDepthBuffer.isDefined ) pipeline.bindDepthBuffer( nameForDepthBuffer.get, depthFBO )
    for( (fbo,name) <- usedFrameBufferObjects ) pipeline.bindColorBuffer( name, fbo, 0 )

    val shaderMaterial = getShaderMaterial
    for( (name, image) <- images )
      shaderMaterial.setTexture( nameOfEffect.get, name, image )

    pipeline.drawQuad( shaderMaterial, nameOfEffect.get )
    switchFBO
  }

  /**
   * This is a small helper class for the post processing effect domain specific language.
   *
   * @author Stephan Rehfeld
   *
   * @param fboName The name of the frame buffer.
   * @param ppe The post processing effect.
   */
  class FrameBufferMapper( val fboName : String, val ppe : PostProcessingEffect ) {
    require( fboName != null, "The parameter 'fboName' must not be 'null'!" )
    require( fboName != "", "The parameter 'fboName' must not an empty string'!" )
    require( ppe != null, "The parameter 'ppe' must not be 'null'!" )

    /**
     * This method sets the name under which the frame buffer object should be provided to the shader.
     *
     * @param targetName The name under which the frame buffer object should be provided to the shader.
     * @return The altered post processing effect.
     */
    def as( targetName : String ) : PostProcessingEffect = {
      require( targetName != null, "The parameter 'targetName' must not be 'null'!" )
      require( targetName != "", "The parameter 'targetName' must not an empty string'!" )
      usedFrameBufferObjects = usedFrameBufferObjects + (fboName -> targetName )
      ppe
    }

  }

  /**
   * This is a small helper class for the post processing effect domain specific language.
   *
   * @author Stephan Rehfeld
   *
   * @param tex The texture
   * @param ppe The post processing effect.
   */
  class TextureMapper( val tex : Texture2D, val ppe : PostProcessingEffect ) {
    require( tex != null, "The parameter 'tex' must not be 'null'!" )
    require( ppe != null, "The parameter 'ppe' must not be 'null'!" )

    def this(file : String, ppe : PostProcessingEffect) =
      this(ResourceManager.loadTexture( new File( file ) ), ppe)


    /**
     * This method sets the name under which the texture should be provided to the shader.
     *
     * @param targetName The name under which the texture should be provided to the shader.
     * @return The altered post processing effect.
     */
    def as( targetName : String ) : PostProcessingEffect = {
      require( targetName != null, "The parameter 'targetName' must not be 'null'!" )
      require( targetName != "", "The parameter 'targetName' must not an empty string'!" )
      images = images + (targetName -> tex)
      ppe
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
    for( uniformManager <- this.uniformList ) {
      if( uniformManager.ontologyMember.isDefined ) {
        sVarDescriptions = sVarDescriptions ::: uniformManager.ontologyMember.get :: Nil
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
    require( sVarDescription != null, "The parameter 'sVarDescription' must not be 'null'!" )
    var value : Option[T] = None
    for( uniformManager <- this.uniformList ) {
      if( uniformManager.ontologyMember.isDefined && uniformManager.ontologyMember.get == sVarDescription ) {
        value = Some( uniformManager.ontologyMember.get.defaultValue().asInstanceOf[T] )
      }
    }
    value.get

  }

  def getCreateParameters : Seq[SVal[_]] = {
    def combine[T, U](um : UniformManager[T, U, _]) =
      um.ontologyMember.get.apply(um.converter._2(um.value))

    uniformList.filter(_.ontologyMember.isDefined).map( combine(_) )
  }

  /**
   * This method sets the time since the last fram in seconds.
   *
   * @param deltaT The time since the last frame in seconds.
   */
  private[jvr] def setCurrentDeltaT( deltaT : Float ) {
    if( this.deltaTName.isDefined ) {
      this.getShaderMaterial.setUniform( this.nameOfEffect.get, this.deltaTName.get, new UniformFloat( deltaT ) )
    }
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
 * The companion object of the post processing effect.
 *
 * @see [[simx.components.renderer.jvr.PostProcessingEffect]]
 *
 * @author Stephan Rehfeld
 */
object PostProcessingEffect {

  /**
   * The method to start a description of a post processing effect.
   *
   * @param nameOfEffect The name of the post processing effect.
   * @return The post processing effect.
   */
  def apply( nameOfEffect : String ) : PostProcessingEffect = {
    require( nameOfEffect != null, "The parameter 'nameOfEffect' must not be 'null'!" )
    val e = new PostProcessingEffect
    e.nameOfEffect = Some( nameOfEffect )
    e
  }

}