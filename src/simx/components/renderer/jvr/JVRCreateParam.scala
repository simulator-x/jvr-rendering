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

import de.bht.jvr.core._
import java.io.File
import simx.core.components.renderer.createparameter.{ReadFromElseWhere, RendererAspect}
import simx.core.ontology.types.{ColladaFile, Transformation, Scale, SubElement}
import simx.core.entity.Entity
import simx.core.ontology.{SValDescription, GroundedSymbol, Symbols}
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}
import simx.core.entity.description.{SVal, SValSeq}
import simx.core.helper.Execute
import simx.core.svaractor.SVarActor
import simplex3d.math.floatx.{Mat4f, ConstMat4f}
import simx.core.svaractor.semantictrait.base.{Thing, Base}

/**
 * This is a common base class for all jVR specific create parameter.
 *
 * @param aspectType The aspect type of this create parameter.
 * @param targets The targets of this aspect. Default value is Nil.
 */
private[jvr] abstract class JVRAspect( aspectType : GroundedSymbol, targets : List[Symbol] = Nil )
  extends RendererAspect( aspectType, targets ) {
  require( aspectType != null, "The parameter 'aspectType' must not be 'null'!" )
  require( targets != null, "The parameter 'targets' must not be 'null'!" )
}

/**
 * An internal helper class to create a sky box scene graph object.
 *
 * @author Stephan Rehfeld
 *
 * @param name Name of the sky box
 * @param backTexture Texture for the back side of the sky box.
 * @param bottomTexture Texture for the down side of the sky box.
 * @param frontTexture Texture for the front side of the sky box.
 * @param leftTexture Texture for the left side of the sky box.
 * @param rightTexture Texture for the right side of the sky box.
 * @param topTexture Texture for the up side of the sky box.
 */
private[jvr] case class SkyBoxCreator(
                                       name: String,
                                       backTexture: String,
                                       bottomTexture: String,
                                       frontTexture: String,
                                       leftTexture: String,
                                       rightTexture: String,
                                       topTexture: String
                                       ) {
  require( name != null, "The parameter 'name' must not be 'null'!" )
  require( backTexture != null, "The parameter 'backTexture' must not be 'null'!" )
  require( backTexture != "", "The parameter 'backTexture' must not be an empty string!" )
  require( bottomTexture != null, "The parameter 'bottomTexture' must not be 'null'!" )
  require( bottomTexture != "", "The parameter 'bottomTexture' must not be an empty string!" )
  require( frontTexture != null, "The parameter 'frontTexture' must not be 'null'!" )
  require( frontTexture != "", "The parameter 'frontTexture' must not be an empty string!" )
  require( leftTexture != null, "The parameter 'leftTexture' must not be 'null'!" )
  require( leftTexture != "", "The parameter 'leftTexture' must not be an empty string!" )
  require( rightTexture != null, "The parameter 'rightTexture' must not be 'null'!" )
  require( rightTexture != "", "The parameter 'rightTexture' must not be an empty string!" )
  require( topTexture != null, "The parameter 'topTexture' must not be 'null'!" )
  require( topTexture != "", "The parameter 'topTexture' must not be an empty string!" )

  /**
   * The file for the used plane geometry.
   */
  private val planeFile  = new File( "models/plane.dae" )

  /**
   * The shaders that are used to render the sky box.
   */
  private val skyShaders = List("shader/default_ambient.vs", "shader/sky.fs")

  /**
   * A map that hold the transformation for each side of the sky box.
   */
  private val transforms = Map(
    new File(frontTexture)  -> Transform.translate(0,     0,    -0.5f),
    new File(rightTexture)  -> Transform.translate(-0.5f, 0,     0)    .mul(Transform.rotateYDeg( 90)),
    new File(leftTexture)   -> Transform.translate(0.5f,  0,     0    ).mul(Transform.rotateYDeg(-90)),
    new File(backTexture)   -> Transform.translate(0,     0,     0.5f) .mul(Transform.rotateYDeg(180)),
    new File(topTexture)    -> Transform.translate(0,     0.5f,  0)    .mul(Transform.rotateYDeg(90)).mul(Transform.rotateXDeg( 90)),
    new File(bottomTexture) -> Transform.translate(0,    -0.5f,  0)    .mul(Transform.rotateYDeg(180)).mul(Transform.rotateXDeg(-90))
  )

  /**
   * This method creates shape node for one side of the sky box.
   *
   * @param shaderContext The name of the shader context where the sky box is rendered in.
   * @param transform The transform of the side to create.
   * @param tex The texture of the side.
   * @param textureProg The shader program that is used to render this side of the sky box.
   * @param planeGeo The plane geometry for this side of the sky box.
   * @return The shape node for this side of the sky box.
   */
  private def createPart( shaderContext : String, transform : Transform, tex : Texture2D, textureProg : ShaderProgram, planeGeo : Geometry ) = {
    require( shaderContext != null, "The parameter 'shaderContext' must not be 'null'!" )
    require( transform != null, "The parameter 'transform' must not be 'null'!" )
    require( tex != null, "The parameter 'tex' must not be 'null'!" )
    require( textureProg != null, "The parameter 'textureProg' must not be 'null'!" )
    require( planeGeo != null, "The parameter 'planeGeo' must not be 'null'!" )

    val mat = new ShaderMaterial(shaderContext, textureProg)
    mat.setTexture(shaderContext, "jvr_Texture0", tex)
    new ShapeNode("", planeGeo, mat).setTransform(transform)
  }

  /**
   * This method creates a sky box scene graph node.
   *
   * @param shaderContext The shader context where the sky box should be rendererd in.
   * @return The scene graph node that contains the sky box.
   */
  def create( shaderContext : String )(implicit self : SVarActor) : SceneNode = {
    require( shaderContext != null, "The parameter 'shaderContext' must not be 'null'!" )

    val resultSky = new GroupNode()

    Execute inParallel self.ask[GroupNode](ResourceManager.self, AskColladaFile(planeFile)) and
      self.ask[ShaderProgram](ResourceManager.self, AskShaderProgram(skyShaders.map(new File(_)))) and
      self.ask[Seq[(File,Texture2D)]](ResourceManager.self, AskTextureFiles(transforms.keys)) exec
      {
        _ match {
          case (((_, plane : GroupNode), program : ShaderProgram), texseq : Seq[(File,Texture2D)]) =>
            val geom = Finder.findGeometry( plane, null )
            val skyBox = new GroupNode("SkyBox")
            texseq.foreach{ m => skyBox.addChildNode(createPart(shaderContext, transforms(m._1), m._2, program, geom)) }
            resultSky.addChildNode(skyBox.setTransform(Transform.scale( 100f ).mul(Transform.rotateYDeg(180))))
            resultSky.setName( name )
        }
      }
    resultSky
  }
}

/**
 * This create parameters takes a [[simx.components.renderer.jvr.PostProcessingEffect]] as parameters. This effect
 * is constructed and the parameters are provided an SVars.
 *
 * @param ppe The description of the [[simx.components.renderer.jvr.PostProcessingEffect]].
 */
case class PPE( ppe : PostProcessingEffect ) extends RendererAspect( Symbols.postProcessingEffect ) {

  require( ppe != null, "The parameter 'ppe' must not be null!" )

  val list = ppe.getSVarDescriptions

  override def getCreateParams = addCVars {
    ppe.getCreateParameters ++ (simx.components.renderer.jvr.ontology.types.PostProcessingEffect( ppe ) :: Nil)
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    for( sVarDescription <- list ) {
      features = features + sVarDescription
    }
    features
  }
}

/**
 * This craeate parameter is a jVR specific adaption of the general
 * [[simx.core.components.renderer.createparameter.ExistingNode]] create parameter. Beside the standard parameter is
 * allows to manipulate the shader program of the node by applying a [[simx.components.renderer.jvr.ShaderEffect]].
 *
 * @param subElement The name of the element in the scene graph.
 * @param scale A scale factor for this element.
 * @param shaderEffect The shader effect that should be applied to the element.
 */
case class JVRExistingNode(
                            subElement: String,
                            scale : ConstMat4f = Mat4f.Identity,
                            shaderEffect : ShaderEffect
                            ) extends RendererAspect( Symbols.existingNode ) {

  require( subElement != null, "The parameter 'subElement' must not be null!" )
  require( subElement != "", "The parameter 'subElement' must not be an empty string!" )
  require( scale != null, "The parameter 'scale' must not be null!" )
  require( shaderEffect != null, "The parameter 'shaderEffect' must not be null!" )

  override def getCreateParams = {
    val seq = new SValSeq(SubElement( subElement )) and Scale( scale ) and ontology.types.ShaderEffect( shaderEffect )
    myCombine.foldLeft(seq){ _ and _ }
    addCVars(seq)
  }

  private def myCombine = {
    def toSVal[T, B](in : SValDescription[T, B, _ <: Base, _ <: Thing]) : SVal[T,TypeInfo[T,T], _ <: Base, _ <: Thing] =
      in.apply(shaderEffect.getValueForSVarDescription(in))

    shaderEffect.getSVarDescriptions.map(x => toSVal(x))
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features +  Transformation
    features = features ++ shaderEffect.getSVarDescriptions
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    providings = providings +  Transformation
    providings = providings ++ shaderEffect.getSVarDescriptions
    providings
  }

}

/**
 * This craeate parameter is a jVR specific adaption of the general
 * [[simx.core.components.renderer.createparameter.ShapeFromFile]] create parameter. Beside the standard parameter it
 * allows to manipulate the shader program of the node by applying a [[simx.components.renderer.jvr.ShaderEffect]].
 *
 * @param file The file name to load.
 * @param subElement Optional the name of a sub element in the loaded scene graph that should be representation of the entity.
 * @param parentElement An optional entity that represents paranent element in the scene graph.
 * @param transformation The transformation of the element for ReadFromElseWhere if another component provides this parameter. Default value is the identity matrix.
 * @param scale A scale factor for the element. Default value is no scale factor.
 * @param shaderEffect The shader effect that should be applied to the element.
 */
case class JVRShapeFromFile( file: String,
                             subElement : Option[String] = None,
                             parentElement : Option[Entity] = None,
                             transformation : Either[ReadFromElseWhere,ConstMat4f]  = Right( Mat4f.Identity  ),
                             scale : ConstMat4f = Mat4f.Identity,
                             shaderEffect : ShaderEffect
                             ) extends JVRAspect( Symbols.shapeFromFile ) {

  require( file != null, "The parameter 'file' must not be null!" )
  require( file != "", "The parameter 'file' must not be an empty string!" )
  require( subElement != null, "The parameter 'subElement' must not be null!" )
  if( subElement.isDefined) require( subElement.get != "", "The parameter 'subElement' must not contain an empty string!" )
  require( transformation != null, "The parameter 'transformation' must not be null!" )
  require( scale != null, "The parameter 'scale' must not be null!" )
  require( shaderEffect != null, "The parameter 'shaderEffect' must not be null!" )

  override def getCreateParams = {
    var cVars = new SValSeq
    cVars = cVars and ColladaFile( file )
    if( subElement.isDefined ) cVars = cVars and SubElement( subElement.get )
    if( transformation.isRight ) cVars = cVars and Transformation( transformation.right.get )
    cVars = cVars and Scale( scale ) and ontology.types.ShaderEffect( shaderEffect )
    myCombine.foldLeft(cVars){ _ and _ }
    addCVars( cVars )
  }

  private def myCombine = {
    def toSVal[T, B](in : SValDescription[T, B, _ <: Base, _ <: Thing]) : SVal[T,TypeInfo[T,T], _ <: Base, _ <: Thing] =
      in.apply(shaderEffect.getValueForSVarDescription(in))

    shaderEffect.getSVarDescriptions.map(x => toSVal(x))
  }

  override def getFeatures : Set[ConvertibleTrait[_]] = {
    var features = Set[ConvertibleTrait[_]]()
    features = features + Transformation
    features = features ++ shaderEffect.getSVarDescriptions
    features
  }

  override def getProvidings : Set[ConvertibleTrait[_]] = {
    var providings = Set[ConvertibleTrait[_]]()
    if( transformation.isRight ) providings = providings +  Transformation
    providings = providings ++ shaderEffect.getSVarDescriptions
    providings
  }
}