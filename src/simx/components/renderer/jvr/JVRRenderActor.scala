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
import pipeline.{PipelineCommandPtr, Pipeline}
import simx.core.entity.Entity
import de.bht.jvr.renderer._
import java.io.File
import de.bht.jvr.util.Color
import simx.core.entity.description._
import uniforms._
import de.bht.jvr.input.{MouseEvent, MouseListener, KeyEvent, KeyListener}
import javax.media.opengl.GL2GL3
import de.bht.jvr.core._
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.components.io.IODeviceProvider
import simx.core.svaractor.{SVar, SVarImpl, SVarActor}
import de.bht.jvr.math.{Vector2, Vector4}
import simx.core.ontology.{types => oTypes, Symbols}
import simx.core.helper.{TextureData, SVarUpdateFunctionMap}
import simx.core.component._
import simx.core.component.Triggered
import simx.core.component.Unbound
import simx.core.entity.description.EntityConfiguration
import simx.core.component.ObserveStepCount
import simplex3d.math.floatx.{Vec3f, Vec2f, ConstVec3f, ConstVec4f, functions}
import scala.reflect.ClassTag


/**
 * A JVRRenderActor manages a scene graph, constructs a rendering pipeline and renders to one or more windows.
 *
 * It is started by the [[simx.components.renderer.jvr.JVRConnector]] depending on the passed DisplayDescription.
 *
 * @author Stephan Rehfeld
 */
class JVRRenderActor extends SVarActor with SVarUpdateFunctionMap  with IODeviceProvider {

  /**
   * The id of the render actor.
   */
  var id = -1

  /**
   * A map that maps from entity to scene graph node.
   */
  var entityToNodeMap : Map[Entity, List[GroupNode]] = Map()

  var entityToDirectParentOfShape : Map[Entity, GroupNode] = Map()

  private var shaderEffectMap = Map[ShapeNode, ShaderEffect]()

  /**
   * The stereo mode if this actor manages a windows with stereo capability.
   */
  var stereoMode : Option[StereoMode.Value] = None

  /**
   * A flag, if shadow are enabled or disabled.
   */
  var shadows = false

  /**
   * The quality of the shadows.
   */
  var shadowQuality = "None"

  /**
   * The quality of the frame buffer for mirror surfaces.
   */
  var mirrorQuality = "None"

  /**
   * The time, when the last frame finished.
   */
  var lastFrame = System.currentTimeMillis()

  /**
   * The scene root.
   */
  val sceneRoot = new GroupNode("Root")

  /**
   * A map of the cameras. Typically one for each window and /or eye.
   */
  var cameras = Map[RenderWindow,Map[Symbol,VRCameraNode]]()

  /**
   * A list of all cameras.
   */
  var camList = Map[RenderWindow,List[VRCameraNode]]()

  /**
   * The mirror camreas.
   */
  var mirrorCameras = Map[RenderWindow,Map[VRCameraNode,Map[Symbol,VRCameraNode]]]()

  /**
   * This list contains al update object that updates mirror cameras to keep them in sync with the reference camera
   * and the effect plane.
   */
  var cameraUpdater = List[CameraUpdater]()

  /**
   * The sky boxes of the cameras.
   */
  var skyBoxes : Map[VRCameraNode,SceneNode] = Map()

  /**
   * The creator of the sky box. It is used to create a sky box for the mirror cameras.
   */
  var skyBoxCreator : Option[SkyBoxCreator] = None

  /**
   * The render windows managed my this render actor.
   */
  var windows : List[RenderWindow] = List()

  /**
   * The viewer that controls the render windows.
   */
  var viewer : Option[Viewer] = None

  /**
   * The render pipelines of the manages windows.
   */
  var pipelines : Map[RenderWindow,Pipeline] = Map()

  /**
   * Pipeline pointer to the command to change the ambient light for all managed windows.
   */
  var ambientUniforms : Map[RenderWindow, PipelineCommandPtr] = Map()

  /**
   * This list contains the names of all effect planes like mirros, water and portals.
   */
  var effectplanes : List[String] = List()

  /**
   * This list contains all water materials.
   */
  var waterMats : Map[String,ShaderMaterial] = Map()

  /**
   * The time where the actor has been created. Used for the computation of some effects.
   */
  val t0 = System.currentTimeMillis()

  /**
   * This map holds prepared scene graph element that will be inserted into the scene graph after the configuration of
   * the entity completed.
   */
  var toInsert = Map[Entity, List[Entity => Unit]]()

  /**
   * Helper type for io devices.
   *
   * tparam T The type of a channel of the io device.
   */
  private type TypedTuple[T] = (ConvertibleTrait[T], SVar[T])

  /**
   *  A map used for the keyboard representation.
   */
  val keyboardMap = scala.collection.mutable.Map[Int, Boolean => Unit]()

  /**
   * A map used for the keyboard representation.
   */
  val mouseMappings = scala.collection.mutable.Set[TypedTuple[_]]()

  /**
   * A list of all active [[simx.components.renderer.jvr.PostProcessingEffect]]s currently active.
   */
  var postProcessingEffects : List[PostProcessingEffect] = List()

  /**
   * A map from entity to the [[simx.components.renderer.jvr.PostProcessingEffect]]s of this entity.
   */
  var entityToPostProcessingEffects : Map[Entity,List[PostProcessingEffect]] = Map()

  /**
   * The frequency of this render actor.
   */
  var frequency : Frequency = Triggered()

  /**
   * Helper function that is used for observer of the view platform transform. Sets the transform to all cameras.
   *
   * @param transform The new transformation of the view platform.
   */
  def setTransform( transform : Transform ) {
    require( transform != null, "The parameter 'transform' must not be 'null'!" )
    for( (window,c) <- camList ) {
      for( camera <- c ) {
        camera.setTransform( transform )
      }
    }

  }

  /**
   * Helper function that is used for observer of the head transform. Sets the head transform to all cameras.
   *
   * @param transform The new head transformation.
   */
  def setHeadTransform( transform : Transform ) {
    require( transform != null, "The parameter 'transform' must not be 'null'!" )
    for( (window,c) <- camList ) {
      for( camera <- c ) {
        camera.setHeadTransform( transform )
      }
    }
  }

  /**
   * Helper variable to be used in handler.
   */
  var me = this

  addHandler[DetachObject]{
    msg => entityToNodeMap( msg.e ).foreach{
      entity => entity.getParentNode.asInstanceOf[GroupNode].removeChildNode( entity )
    }
  }

  addHandler[AttachObject]{
    msg => entityToNodeMap( msg.e ).foreach(sceneRoot.addChildNode(_))
  }

  addHandler[RegroupEntity] {
    msg =>
      val t  = {
        msg.target match {
          case Some( entity ) => entityToNodeMap( entity ).head
          case None => sceneRoot
        }
      }

      val entities = entityToNodeMap( msg.e )
      entities.foreach {
        entity => if( msg.convertTransform ) {
          val worldTransform = entity.getWorldTransform( sceneRoot )
          val worldTransformOfTarget = t.getWorldTransform( sceneRoot )
          val newTransform = worldTransformOfTarget.invert.mul( worldTransform )
          entity.setTransform( newTransform )
          set( msg.e.get( types.Transformation ).head, newTransform )
        }

          entity.getParentNode.asInstanceOf[GroupNode].removeChildNode( entity )
          t.addChildNode( entity )
      }
      msg.sender ! RegroupApplied(msg.e, msg.target)
  }

  /**
   * This flag shows if the user entity has already been registered.
   */
  private var userEntityIsRegistered = false

  addHandler[JVRPublishUserEntity] {
    msg: JVRPublishUserEntity =>
      if(!userEntityIsRegistered){
        val viewPlatform = msg.user.get(types.ViewPlatform).head
        val headTransform = msg.user.get(types.HeadTransform).head

        observe( viewPlatform)( setTransform )
        get( viewPlatform)( setTransform )
        observe( headTransform)( setHeadTransform )
        get( headTransform)( setHeadTransform )
        userEntityIsRegistered = true
      }
  }

  addHandler[RequestTransformation]{
    msg =>
      val n : SceneNode = Finder.find( sceneRoot, classOf[SceneNode], msg.element )
      val worldTransform = n.getWorldTransform( sceneRoot )
      msg.sender ! TellTransformation( msg.entity, worldTransform, msg.aspect )
  }

  addHandler[RenderActorConfigs] {
    configs : RenderActorConfigs =>
      var winId = 0

      frequency = configs.frequency
      shadows = !configs.shadowQuality.equals( "none" )
      shadowQuality = configs.shadowQuality
      mirrorQuality = configs.mirrorQuality

      for( config <- configs.configs) {
        val pipeline = new Pipeline( sceneRoot )

        val window = config.resolution match {
          case Some((width, height)) =>
            new NewtRenderWindow( pipeline, width, height )
          case None =>
            //if(JVMTools.isWindows) new NewtRenderWindow( pipeline, true )
            //    else
            new NewtRenderWindow( pipeline, true )
        }

        config.hardwareHandle.collect{ case (_, screen, _) => window.setScreenDevice( screen ) }
        window.setVSync( true )

        config.stereoMode match {
          case Some( StereoMode.FrameSequential ) =>
            window.setStereo( true )
          case Some(_)  =>
          case None     => window.setStereo( false )
        }

        mirrorCameras = mirrorCameras + (window -> Map())
        cameras       = cameras + (window -> Map())
        camList       = camList + (window -> List())

        def createCam(c : RenderActorConfig, isLeft : Option[Boolean]) : VRCameraNode = {
          val cam = new VRCameraNode(
            c.id.toString + isLeft.collect{ case b => if (b) "_left" else "_right" }.getOrElse(""),
            JVRConnector.transformConverter.revert( c.transformation ),
            new Vector4((-c.size._1/2f).toFloat, (c.size._1/2f).toFloat, (c.size._2/2f).toFloat, (-c.size._2/2f).toFloat),
            isLeft.getOrElse(c.eyeToRender == EyeToRender.Left),
            Transform.identity()
          )
          c.eyeSeparation.collect{ case s => cam.setEyeSeparation( s )}
          cam
        }

        if( config.eyeToRender == EyeToRender.Both ) {
          cameras = cameras + (window -> ( cameras( window ) + ('standardLeft  -> createCam(config, Some(true )) ) ) )
          cameras = cameras + (window -> ( cameras( window ) + ('standardRight -> createCam(config, Some(false)) ) ) )
        } else {
          cameras = cameras + (window -> ( cameras( window ) + ('standard -> createCam(config, None) ) ) )
        }
        if( cameras( window ).contains( 'standard ) ) {
          sceneRoot.addChildNode( cameras( window )( 'standard ) )
          camList = camList + (window -> (camList(window) ::: cameras( window )( 'standard ) :: Nil))
        } else {
          sceneRoot.addChildNode( cameras( window )( 'standardLeft ) )
          sceneRoot.addChildNode( cameras( window )( 'standardRight ) )
          camList = camList + (window -> (camList(window) ::: cameras( window )( 'standardLeft ) :: Nil))
          camList = camList + (window -> (camList(window) ::: cameras( window )( 'standardRight ) :: Nil))
          stereoMode = config.stereoMode
        }

        createDefaultPipeline( pipeline, cameras( window ), sceneRoot, window )

        pipelines  = pipelines + (window -> pipeline )

        def nameIt( n : String, e : Entity ) = e.injectSVar(SVarImpl(n + " (" + id + ", " + winId + ")"), oTypes.Name)

        val kb = nameIt("Keyboard", oTypes.Mappings.keyMap.foldLeft(new Entity){ (kb, tuple) =>
          val svar = SVarImpl(false)
          keyboardMap.update( tuple._1, set( svar, _ ) )
          kb.injectSVar( svar, tuple._2 )
        })

        def toTuple[T](a : ConvertibleTrait[T], b : SVar[T]) : TypedTuple[_] = (a, b).asInstanceOf[TypedTuple[_]]

        mouseMappings += toTuple(oTypes.Origin, SVarImpl(Vec3f.Zero))
        mouseMappings += toTuple(oTypes.Direction, SVarImpl( Vec3f.Zero))
        mouseMappings += toTuple(oTypes.Position2D, SVarImpl( Vec2f.Zero ))
        mouseMappings += toTuple(oTypes.Button_Left, SVarImpl( false ))
        mouseMappings += toTuple(oTypes.Button_Right, SVarImpl( false ))
        mouseMappings += toTuple(oTypes.Button_Center, SVarImpl( false ))

        def inject[T](e : Entity, tuple : TypedTuple[T]) : Entity = e.injectSVar(tuple._2, tuple._1)
        val mouseEntity = nameIt("Mouse", mouseMappings.foldLeft(new Entity){ (a,b) => inject(a, b) })

        registerListeners(window, config)
        publishDevice( oTypes.Keyboard(kb),  Symbol(config.hardwareHandle.getOrElse((0,0,0))._2.toString) :: Nil)
        publishDevice( oTypes.Mouse( mouseEntity ), Symbol(config.hardwareHandle.getOrElse((0,0,0))._2.toString) :: Nil )
        GeneralEntityDescription.notifyObservers( EntityConfiguration(mouseEntity, Map()))
        GeneralEntityDescription.notifyObservers( EntityConfiguration(kb, Map()))

        windows = windows ::: window :: Nil
        winId += 1
      }
      viewer = Some( new Viewer( true , windows : _* ) )
      viewer.get.display()
      sender ! JVRRenderActorConfigComplete( )

  }

  /**
   * The parent jvr connector that created this render actor.
   */
  private var jvrConnector : Option[SVarActor.Ref] = None

  /*
   * This indirection is necessary, as the listener are executed within the actor proxy and not within this actor.
   * This causes an error due serialization.
   */
  addHandler[JVRRenderWindowClosed] {
    msg =>
      jvrConnector.get ! JVRRenderWindowClosed()
      me.shutdown()
  }

  addHandler[SwitchEyes]{
    msg =>
      camList.foreach(_._2.foreach(vrcamnode => {vrcamnode.setEyeSeparation(vrcamnode.getEyeSeparation * -1f)}))
  }

  addHandler[PinToCam]{ msg =>
    val offset = JVRConnector.transformConverter.revert(msg.offset)
    camList.foreach(cams => {
      cams._2.foreach(cam => {
        entityToNodeMap.get(msg.e).collect{case node =>
          val g = new GroupNode("PinToCamOffset")
          g.setTransform(offset)
          g.addChildNode(cam)
          node.head.addChildNode(g)
        }
      })
    })
  }

  private var windowClosed = false

  /**
   * This helper function registers listener to the windows of this render actor to provide the input of mouse and
   * keyboard.
   *
   * @param window The windows.
   * @param config The configuration of the render window.
   */
  private def registerListeners( window : RenderWindow, config : RenderActorConfig ) {
    require( window != null, "The parameter 'window' must not be 'null'!" )
    require( config != null, "The parameter 'config' must not be 'null'!" )

    jvrConnector = Some( config.sender )

    window.addWindowListener{
      new WindowListener {
        override def windowReshape(p1: RenderWindow, p2: Int, p3: Int) {}
        override def windowClose(p1: RenderWindow) {
          windowClosed = true
          me.self ! JVRRenderWindowClosed( )
        }
      }
    }

    window.addKeyListener{
      new KeyListener {
        override def keyTyped(p1: KeyEvent) {}
        override def keyReleased(p1: KeyEvent){ keyboardMap.get(p1.getKeyCode).collect{ case f => f(false)} }
        override def keyPressed( p1: KeyEvent){ keyboardMap.get(p1.getKeyCode).collect{ case f => f(true) } }
      }
    }

    window.addMouseListener( new MouseListener {
      val buttons = Map[Int, ConvertibleTrait[Boolean]](1 -> oTypes.Button_Left, 2 -> oTypes.Button_Center, 3 -> oTypes.Button_Right)
      def sett[T]( sval : SVal[T] ){ mouseMappings.find( _._1 == sval.typedSemantics).collect{ case t => set( t._2, sval.as(t._1)) } }

      def mouseReleased(e: MouseEvent) {
        buttons.get(e.getButton).collect{ case x => sett(x(false)) }
        updatePickRay(e)
      }

      def mousePressed(e: MouseEvent) {
        buttons.get(e.getButton).collect{ case x => sett(x(true)) }
        updatePickRay(e)
      }

      def mouseMoved( e: MouseEvent ) {
        sett( oTypes.Position2D(Vec2f( e.getX.toFloat, e.getY.toFloat )) )
      }

      def mouseClicked(e: MouseEvent) {
        updatePickRay(e)
      }

      def mouseExited(e: MouseEvent)    {}
      def mouseEntered(e: MouseEvent)   {}
      def mouseDragged(e: MouseEvent)   {sett( oTypes.Position2D(Vec2f( e.getX.toFloat, e.getY.toFloat )) )}
      def mouseWheelMoved(e: MouseEvent){}

      private def updatePickRay(e: MouseEvent) {
        val pickRay = Picker.getPickRay(sceneRoot, cameras.head._2.head._2, e.getNormalizedX, e.getNormalizedY)
        sett(oTypes.Origin(ConstVec3f(pickRay.getRayOrigin.x, pickRay.getRayOrigin.y, pickRay.getRayOrigin.z)))
        sett(oTypes.Direction(ConstVec3f(pickRay.getRayDirection.x, pickRay.getRayDirection.y,pickRay.getRayDirection.z)))
      }
    })
  }

  addHandler[SetAmbientColor]{
    msg =>
      for((window, ptr) <- ambientUniforms) {
        ptr.setUniform("jvr_Material_Ambient", new UniformColor(msg.color))
      }
  }

  var frameBegins = List[Long]()

  addHandler[RenderNextFrame] {
    case msg =>
      if( remoteStepCount.isDefined )
        println( remoteStepCount.get )
      if(JVRDebugSettings.printFPS) {
        if( frequency != Triggered() ) {
          if( frameBegins.size > 1 ) {
            if( (frameBegins.reverse.head - frameBegins.head) > 10000 ) {
              val time = (frameBegins.reverse.head - frameBegins.head).asInstanceOf[Double] / 1000.0
              println( frameBegins.size.asInstanceOf[Double] / time )
              frameBegins = List()
            }
          }
          frameBegins = frameBegins ::: System.currentTimeMillis() :: Nil
        }
      }
      for( updater <- cameraUpdater ) updater.update()
      for( (name,waterMat) <- waterMats )
        waterMat.setUniform( name + "_pass", "waveTime", new UniformFloat( ((System.currentTimeMillis() -t0) * 1e-4).asInstanceOf[Float] ))
      for( ( camera, s ) <- skyBoxes ) {
        s.setTransform( camera.getEyeWorldTransform( sceneRoot ).extractTranslation )
      }

      if( viewer.isEmpty ) println("Viewer is not set!" )
      val deltaT = (System.nanoTime - this.lastFrame) / 1000.0f
      for( ppe <- this.postProcessingEffects ) ppe.setCurrentDeltaT( deltaT )

      viewer.collect{ case v if !windowClosed => v.display() }
      if( frequency == Triggered() ) msg.sender ! FinishedFrame()
      if( frequency == Unbound() ) self ! RenderNextFrame()
      this.lastFrame = System.nanoTime
  }



  addHandler[PublishSceneElement] {
    msg =>
      msg.aspect.getCreateParams.semantics match {
        case Symbols.existingNode         => createExistingNode( msg.sender, msg.e, msg.aspect )
        case Symbols.mirror               => createMirror( msg.sender, msg.e, msg.aspect, msg.providings )
        case Symbols.pointLight           => createPointLight( msg.sender, msg.e, msg.aspect, msg.providings )
        case Symbols.postProcessingEffect => createPostProcessingEffect( msg.sender, msg.e, msg.aspect )
        case Symbols.shapeFromFile        => createShapeFromFile( msg.sender, msg.e, msg.aspect, msg.providings )
        case Symbols.skyBox               => createSkyBox( msg.sender, msg.e, msg.aspect )
        case Symbols.spotLight            => createSpotLight( msg.sender, msg.e, msg.aspect, msg.providings )
        //        case Symbols.texture              => createTexture()
        case Symbols.water                => createWater( msg.sender, msg.e, msg.aspect, msg.providings )
        case x =>
      }
  }

  addHandler[RemoveSceneElement] {
    msg =>
      val entities   = entityToNodeMap( msg.e )
      val parentNode = entities.head.getParentNode.asInstanceOf[GroupNode]
      if (parentNode != null)
        entities.foreach(parentNode.removeChildNode(_))

      msg.e.getAllSVars.foreach( triple => ignoreMultiobserve(triple._3) )
  }

  /**
   * This constructs the pipeline for the render window.
   *
   * @param pipeline The pipeline that should be filled.
   * @param cameras A map of the cameras that should be used.
   * @param sceneRoot The root of the scene graph.
   */
  private def createDefaultPipeline( pipeline: Pipeline, cameras: Map[Symbol,VRCameraNode], sceneRoot: GroupNode, window : RenderWindow ) = {
    require( pipeline != null, "The parameter 'pipeline' must not be 'null'!" )
    require( cameras != null, "The parameter 'cameras' must not be 'null'!" )
    require( sceneRoot != null, "The parameter 'sceneRoot' must not be 'null'!" )
    require( window != null, "The parameter 'window' must not be 'null'!" )

    pipeline.setViewFrustumCullingMode( 1 )
    ambientUniforms = ambientUniforms.updated(window,
      pipeline.setUniform("jvr_Material_Ambient", new UniformColor(new Color(0.25f, 0.25f, 0.25f, 1f)))
    )

    if( shadows ) {
      if( shadowQuality.equals( "low" ) ) {
        pipeline.createFrameBufferObject("ShadowMap", true, 0, 512, 512, 0)
      } else if( shadowQuality.equals( "middle" ) ) {
        pipeline.createFrameBufferObject("ShadowMap", true, 0, 1024, 1024, 0)
      } else if( shadowQuality.equals( "high" ) ) {
        pipeline.createFrameBufferObject("ShadowMap", true, 0, 2048, 2048, 0)
      }
    }

    for( fbo <- effectplanes ) {
      if( mirrorQuality.equals( "low" ))
        pipeline.createFrameBufferObject( fbo, false, 1, 0.25f, 0 )
      else if( mirrorQuality.equals( "middle" ))
        pipeline.createFrameBufferObject( fbo, false, 1, 0.75f, 0 )
      else if( mirrorQuality.equals( "high" ))
        pipeline.createFrameBufferObject( fbo, false, 1, 1.0f, 0 )
    }

    pipeline.createFrameBufferObject( "siris_fbo_a", true, 1, 1.0f, 4 )
    pipeline.createFrameBufferObject( "siris_fbo_b", true, 1, 1.0f, 4 )
    pipeline.createFrameBufferObject( "siris_fbo_c", true, 1, 1.0f, 4 )

    if( cameras.contains( 'standard ) ) {
      pipeline.setUniform("jvr_UseClipPlane0", new UniformBool(true))
      pipeline.setUniform("eyeType",
        new UniformFloat(math.signum(cameras.values.foldLeft(0f){ (value, cam) => (if (cam.isLeftEye) -1f else 1f) * cam.getEyeSeparation + value  }))
      )

      for( c <- effectplanes ) {
        simplePipeline( pipeline, mirrorCameras(window)( cameras( 'standard ) ), sceneRoot, Symbol( c ), c )
        if( shadows ) {
          pipeline.switchFrameBufferObject("ShadowMap")
          pipeline.clearBuffers(true, true, null)
        }
      }
      pipeline.setUniform("jvr_UseClipPlane0", new UniformBool(true))
      simplePipeline( pipeline, cameras, sceneRoot, 'standard, "siris_fbo_a" )
      pipeline.switchCamera( cameras( 'standard ) )
      for( c <- effectplanes ) {
        pipeline.bindColorBuffer("jvr_MirrorTexture", c, 0 )
        pipeline.drawGeometry(c + "_pass", null)
      }


      var fboB = true

      pipeline.switchFrameBufferObject( "siris_fbo_b" )
      pipeline.clearBuffers(true, true, new Color(.5f, .5f, .5f))

      pipeline.switchFrameBufferObject( "siris_fbo_c" )
      pipeline.unbindBuffers()
      val printMaterial = new ShaderMaterial( "PRINT", new ShaderProgram( new File( "pipeline_shader/quad.vs" ), new File( "pipeline_shader/default.fs" ) ) )
      pipeline.clearBuffers(true, true, new Color(.5f, .5f, .5f))
      pipeline.bindColorBuffer("jvr_Texture0", "siris_fbo_a", 0)
      pipeline.drawQuad(printMaterial, "PRINT")


      var switchFBO = false
      for( ppe <- postProcessingEffects.sortBy(_.isOverlay) ) {
        if( ppe.isOverlay ) {
          ppe.contributeToPipeline( pipeline, if( fboB ) "siris_fbo_c" else "siris_fbo_b", "siris_fbo_a" )
        } else {
          if( switchFBO ) fboB = !fboB
          pipeline.switchFrameBufferObject( if( fboB ) "siris_fbo_b" else "siris_fbo_c" )
          switchFBO = ppe.contributeToPipeline( pipeline, if( fboB ) "siris_fbo_c" else "siris_fbo_b", "siris_fbo_a" )
        }

      }
      if( switchFBO ) fboB = !fboB

      pipeline.switchFrameBufferObject( null )
      pipeline.unbindBuffers()
      pipeline.clearBuffers(true, true, new Color(.5f, .5f, .5f))
      pipeline.bindColorBuffer("jvr_Texture0", if( fboB ) "siris_fbo_c" else "siris_fbo_b", 0)
      pipeline.drawQuad(printMaterial, "PRINT")


    } else {
      println( "creating eye maps" )
      pipeline.createFrameBufferObject("LeftEyeMap", true, 1, 1.0f, 4)
      pipeline.createFrameBufferObject("RightEyeMap", true, 1, 1.0f, 4)

      pipeline.setUniform("jvr_UseClipPlane0", new UniformBool(true))
      for( c <- effectplanes ) {
        simplePipeline( pipeline, mirrorCameras(window)( cameras( 'standardLeft ) ), sceneRoot, Symbol( c ), c )
        if( shadows ) {
          pipeline.switchFrameBufferObject("ShadowMap")
          pipeline.clearBuffers(true, true, null)
        }
      }

      pipeline.setUniform("jvr_UseClipPlane0", new UniformBool(true))
      pipeline.switchFrameBufferObject( "LeftEyeMap" )
      pipeline.setUniform("eyeType", new UniformFloat(-1))
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))

      simplePipeline( pipeline, cameras, sceneRoot, 'standardLeft, "LeftEyeMap" )
      for( c <- effectplanes ) {
        pipeline.bindColorBuffer("jvr_MirrorTexture", c, 0 )
        pipeline.drawGeometry(c + "_pass", null)
      }

      var fboB = true

      pipeline.switchFrameBufferObject( "siris_fbo_c" )
      pipeline.unbindBuffers()
      val printMaterial = new ShaderMaterial( "PRINT", new ShaderProgram( new File( "pipeline_shader/quad.vs" ), new File( "pipeline_shader/default.fs" ) ) )
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))
      pipeline.bindColorBuffer("jvr_Texture0", "LeftEyeMap", 0)
      pipeline.drawQuad(printMaterial, "PRINT")

      var switchFBO = false
      for( ppe <- this.postProcessingEffects.sortBy(_.isOverlay) ) {
        if( ppe.isOverlay ) {
          ppe.contributeToPipeline( pipeline, if( fboB ) "siris_fbo_c" else "siris_fbo_b", "siris_fbo_a" )
        } else {
          if( switchFBO ) fboB = !fboB
          pipeline.switchFrameBufferObject( if( fboB ) "siris_fbo_b" else "siris_fbo_c" )
          switchFBO = ppe.contributeToPipeline( pipeline, if( fboB ) "siris_fbo_c" else "siris_fbo_b", "siris_fbo_a" )
        }

      }
      if( switchFBO ) fboB = !fboB

      pipeline.switchFrameBufferObject( "LeftEyeMap" )
      pipeline.setUniform("isLeftEye", new UniformBool(true))
      pipeline.unbindBuffers()
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))
      pipeline.bindColorBuffer("jvr_Texture0", if( fboB ) "siris_fbo_c" else "siris_fbo_b", 0)
      pipeline.drawQuad(printMaterial, "PRINT")

      pipeline.unbindBuffers()
      pipeline.setUniform("jvr_UseClipPlane0", new UniformBool(true))
      for( c <- effectplanes ) {
        simplePipeline( pipeline, mirrorCameras(window)( cameras('standardRight) ), sceneRoot, Symbol( c ), c )
        if( shadows ) {
          pipeline.switchFrameBufferObject("ShadowMap")
          pipeline.clearBuffers(true, true, null)
        }
      }

      pipeline.setUniform("jvr_UseClipPlane0", new UniformBool(true))
      pipeline.switchFrameBufferObject( "RightEyeMap" )
      pipeline.setUniform("eyeType", new UniformFloat(1))
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))


      simplePipeline( pipeline, cameras, sceneRoot, 'standardRight, "RightEyeMap" )
      for( c <- effectplanes ) {
        pipeline.bindColorBuffer("jvr_MirrorTexture", c, 0 )
        pipeline.drawGeometry(c + "_pass", null)
      }

      fboB = true

      pipeline.switchFrameBufferObject( "siris_fbo_c" )
      pipeline.unbindBuffers()
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))
      pipeline.bindColorBuffer("jvr_Texture0", "RightEyeMap", 0)
      pipeline.drawQuad(printMaterial, "PRINT")


      switchFBO = false
      for( ppe <- this.postProcessingEffects.sortBy(_.isOverlay) ) {
        if( ppe.isOverlay ) {
          ppe.contributeToPipeline( pipeline, if( fboB ) "siris_fbo_c" else "siris_fbo_b", "siris_fbo_a" )
        } else {
          if( switchFBO ) fboB = !fboB
          pipeline.switchFrameBufferObject( if( fboB ) "siris_fbo_b" else "siris_fbo_c" )
          switchFBO = ppe.contributeToPipeline( pipeline, if( fboB ) "siris_fbo_c" else "siris_fbo_b", "siris_fbo_a" )
        }

      }
      if( switchFBO ) fboB = !fboB

      pipeline.switchFrameBufferObject( "RightEyeMap" )
      pipeline.unbindBuffers()
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))
      pipeline.bindColorBuffer("jvr_Texture0", if( fboB ) "siris_fbo_c" else "siris_fbo_b", 0)
      pipeline.drawQuad(printMaterial, "PRINT")

    }
    if( stereoMode.isDefined && stereoMode.get == StereoMode.AnaglyphStereo ) {
      val anaShader =
        new ShaderProgram(
          new File("pipeline_shader/quad.vs"),
          new File("pipeline_shader/ana.fs")
        )
      val anaMaterial = new ShaderMaterial("ANAGLYPH", anaShader)

      pipeline.switchFrameBufferObject(null)
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))


      pipeline.bindColorBuffer("leftEye", "LeftEyeMap", 0)
      pipeline.bindColorBuffer("rightEye", "RightEyeMap", 0)
      pipeline.drawQuad(anaMaterial, "ANAGLYPH")

    } else if( stereoMode.isDefined && stereoMode.get == StereoMode.FrameSequential ) {

      pipeline.switchFrameBufferObject( null )
      pipeline.unbindBuffers()
      pipeline.setDrawBuffer( GL2GL3.GL_BACK_LEFT )
      val printMaterial = new ShaderMaterial( "PRINT", new ShaderProgram( new File( "pipeline_shader/quad.vs" ), new File( "pipeline_shader/default.fs" ) ) )
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))
      pipeline.bindColorBuffer("jvr_Texture0", "LeftEyeMap", 0)
      pipeline.setUniform("eyeType", new UniformFloat(-1))
      pipeline.drawQuad(printMaterial, "PRINT")

      pipeline.unbindBuffers()

      pipeline.setDrawBuffer( GL2GL3.GL_BACK_RIGHT )
      pipeline.clearBuffers(true, true, new Color(0, 0, 0))
      pipeline.bindColorBuffer("jvr_Texture0", "RightEyeMap", 0)
      pipeline.setUniform("eyeType", new UniformFloat(1))
      pipeline.drawQuad(printMaterial, "PRINT")
    } else if( stereoMode.isDefined && stereoMode.get == StereoMode.TopBottomStereo ) {

      val topBottomStereoMaterial = new ShaderMaterial(
        "topBottomStereo",
        new ShaderProgram(
          new File("pipeline_shader/topBottomStereo.vs"),
          new File("pipeline_shader/topBottomStereo.fs")
        )
      )

      pipeline.switchFrameBufferObject( null )
      pipeline.bindColorBuffer("leftEye", "LeftEyeMap", 0)
      pipeline.setUniform("eyeType", new UniformFloat(-1))
      pipeline.bindColorBuffer("rightEye", "RightEyeMap", 0)
      pipeline.setUniform("eyeType", new UniformFloat(1))
      pipeline.drawQuad(topBottomStereoMaterial, "topBottomStereo")
    }
  }

  /**
   * This function creates a simple pipeline with shadows.
   *
   * @param pipeline The pipeline that should be filled.
   * @param cameras A map of the cameras that should be used.
   * @param sceneRoot The root of the scene graph.
   * @param cam A symbol to the cameras that should be used.
   * @param target The name of the target frame buffer.
   */
  private def simplePipeline( pipeline: Pipeline, cameras: Map[Symbol,VRCameraNode], sceneRoot: GroupNode, cam : Symbol, target: String ) = {

    require( pipeline != null, "The parameter 'pipeline' must not be 'null'!" )
    require( cameras != null, "The parameter 'cameras' must not be 'null'!" )
    require( sceneRoot != null, "The parameter 'sceneRoot' must not be 'null'!" )
    require( cam != null, "The parameter 'cam' must not be 'null'!" )
    require( target != null, "The parameter 'target' must not be 'null'!" )

    pipeline.switchCamera( cameras( cam ) )
    pipeline.switchFrameBufferObject(target)
    pipeline.clearBuffers(true, true, new Color(.5f, .5f, .5f) )
    pipeline.setDepthTest( false )
    pipeline.setBackFaceCulling( false )
    pipeline.drawGeometry( cameras( cam ).getName, null)
    pipeline.setDepthTest( true )
    pipeline.drawGeometry("AMBIENT", null)


    if( shadows ) {
      val lp = pipeline.doLightLoop(false, true)

      lp.switchLightCamera()
      lp.switchFrameBufferObject("ShadowMap")
      lp.clearBuffers(true, false, null)
      lp.drawGeometry("AMBIENT", null)
      lp.switchFrameBufferObject( target )
      lp.switchCamera( cameras( cam ) )
      lp.bindDepthBuffer("jvr_ShadowMap", "ShadowMap" )
      lp.drawGeometry("LIGHTING", null)

      pipeline.doLightLoop(true, false).drawGeometry( "LIGHTING", null )
    } else {
      pipeline.doLightLoop(true, true).drawGeometry( "LIGHTING", null )
    }


  }

  private type ConnectTriple[T] = (ConvertibleTrait[T], T => Any, () => T)
  private def connect( e : Entity, list : ConnectTriple[_]* ) { list.foreach( triple => singleConnect(e, triple, useGet = true ) ) }

  private def singleConnect[T: ClassTag]( entity : Entity, triple : ConnectTriple[T], useGet : Boolean = false ){
    entity.get(triple._1).headOption match {
      case Some(svar) => addMultiobserve(svar, useGet)(triple._2)
      case None       => println("error, could not get " + triple._1)
    }
  }

  private def insertNode( e : Entity, toInsert : SceneNode, trafo : Transform,
                          parentElement : Option[Entity], scale : Option[Transform] = None ) = {
    val hullNode = new GroupNode( toInsert.getName + "_hull")
    hullNode.setTransform( trafo )
    if (scale.isDefined){
      val scaleNode = new GroupNode( toInsert.getName + "_scale")
      scaleNode.setTransform(scale.get)
      scaleNode.addChildNode(toInsert)
      hullNode.addChildNode(scaleNode)
    }
    else
      hullNode.addChildNode( toInsert )
    toInsert.setTransform( new Transform )
    entityToNodeMap = entityToNodeMap.updated(e, hullNode :: entityToNodeMap.getOrElse(e, Nil) )

    parentElement match {
      case Some( gn ) => entityToNodeMap( gn ).head.addChildNode( hullNode )
      case None => sceneRoot.addChildNode( hullNode )
    }

    e.get(simx.core.ontology.types.Texture).collect{
      case textureSVar => entityToDirectParentOfShape.get(e).collect{
        case parentOfShapeNode if parentOfShapeNode.getChildNodes.size == 1 =>
          parentOfShapeNode.getChildNodes.get(0) match {
            case child: GroupNode if child.getNumParentNodes > 1 =>
                parentOfShapeNode.removeChildNode(child)
                parentOfShapeNode.addChildNode(cloneSubTree(e, child))
            case _ =>
          }
          val shapeNode = Finder.find(parentOfShapeNode, classOf[ShapeNode], null)

          def handleTexUpdate(newTexData: TextureData) {
            if(!JVRConnector.isVoidTextureData(newTexData)) {
              shapeNode.getMaterial match {
                case sm : ShaderMaterial =>
                  val newTex = getTextureFor(newTexData)
                  sm.setTexture("AMBIENT", "jvr_Texture0", newTex)
                  sm.setTexture("LIGHTING", "jvr_Texture0", newTex)
                case _ =>
              }
            }}

          textureSVar.observe(handleTexUpdate)
          textureSVar.get( handleTexUpdate _ )
      }
    }

    hullNode
  }

  private var loadedTextures = Map[java.util.UUID, Texture2D]()
  def getTextureFor(d: TextureData): Texture2D = loadedTextures.get(d.id) match {
    case Some(tex) => tex
    case None =>
      //      val newArray = new Array[Byte](d.data.size)
      //      Array.copy(d.data, 0, newArray, 0, d.data.size)
      //      val newTex = new Texture2D(d.size.x, d.size.y, newArray)
      val newTex = new Texture2D(d.size.x, d.size.y, d.data)
      loadedTextures = loadedTextures.updated(d.id, newTex)
      newTex
  }


  private def cloneSubTree(e : Entity, root: GroupNode): SceneNode = {
    val result = new GroupNode(root.getName, root.getTransform)
    val it = root.getChildNodes.iterator()
    while(it.hasNext) it.next() match {
      case g: GroupNode => result.addChildNode(cloneSubTree(e, g))
      case p : LightNode => result.addChildNode(p.getRenderClone)
      case s: ShapeNode => result.addChildNode(cloneShapeNode(e, s))
      case x => throw new Exception(x.getClass.getName + " not supported!")
    }
    result
  }

  private def cloneShapeNode(e : Entity, oldNode : ShapeNode) : ShapeNode = {
    val clonedShapeNode =
      new ShapeNode(oldNode.getName, oldNode.getGeometry, oldNode.getMaterial.getRenderClone, oldNode.getTransform)

    shaderEffectMap.get(oldNode).collect{
      case shaderEffect =>
        shaderEffect.setShaderMaterial(clonedShapeNode.getMaterial.asInstanceOf[ShaderMaterial])
        shaderEffect.bindShaderMaterialToEntity(e)
        shaderEffectMap.updated(clonedShapeNode, shaderEffect)
    }
    clonedShapeNode
  }

  addHandler[InsertEntity]{
    msg => toInsert.get(msg.toInsert) match {
      case Some(funcList) =>
        msg.toInsert.get(oTypes.Name).headOption match {
          case Some(svar) =>
            get( svar)( (name : String) => info("inserting " + name + " into scenegraph") )
            toInsert = toInsert - msg.toInsert
            funcList.foreach( _.apply(msg.toInsert) )
          case None       =>
            info("inserting entity " + msg.toInsert + " into scenegraph" )
            toInsert = toInsert - msg.toInsert
            funcList.foreach( _.apply(msg.toInsert) )
        }
      case None => msg.toInsert.get(oTypes.Name).headOption match {
        case Some(svar) => get( svar)( name => info("entity " + name + " could not be inserted since it was not found"))
        case None => info("entity " + msg.toInsert + " could not be inserted since it was not found")
      }
    }
  }

  private def addInsertion(entity : Entity)(func : Entity => Unit){
    toInsert = toInsert.updated(entity, func :: toInsert.getOrElse(entity, Nil))
  }

  private def asTriple[T](c : ConvertibleTrait[T], x : T => Any, y : () => T) : ConnectTriple[_] =
    (c, x, y).asInstanceOf[ConnectTriple[_]]

  private def createSpotLight( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect, fresh : SValSet ) {
    def fget[T]( c : ConvertibleTrait[T] ) : T  = fresh.firstValueFor(c)

    val parentElement = aspect.getCreateParams.getFirstValueFor( oTypes.ParentElement )
    val transformation = fget( types.Transformation )

    val spotLight = new SpotLightNode( aspect.getCreateParams.firstValueFor( oTypes.Name ) )
    spotLight.setTransform(            transformation )
    spotLight.setCastShadow(           fget( oTypes.CastShadow ) )
    spotLight.setShadowBias(           fget( oTypes.ShadowBias ) )
    spotLight.setSpotCutOff(           fget( oTypes.SpotCutOff ) )
    spotLight.setSpotExponent(         fget( oTypes.SpotExponent ) )
    spotLight.setDiffuseColor(         fget( types.DiffuseColor  ) )
    spotLight.setSpecularColor(        fget( types.SpecularColor ) )
    spotLight.setLinearAttenuation(    fget( oTypes.LinearAttenuation ) )
    spotLight.setConstantAttenuation(  fget( oTypes.ConstantAttenuation ) )
    spotLight.setQuadraticAttenuation( fget( oTypes.QuadraticAttenuation ) )

    addInsertion(entity){ e : Entity =>
      val hullNode = insertNode(e, spotLight, transformation, parentElement)
      connect(e,
        asTriple(types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform),
        asTriple(types.DiffuseColor, spotLight.setDiffuseColor( _ : Color ), spotLight.getDiffuseColor),
        asTriple(types.SpecularColor, spotLight.setSpecularColor( _ : Color ), spotLight.getSpecularColor),
        asTriple(oTypes.ConstantAttenuation, spotLight.setConstantAttenuation( _ : Float ), spotLight.getConstantAttenuation),
        asTriple(oTypes.LinearAttenuation, spotLight.setLinearAttenuation( _ : Float ), spotLight.getLinearAttenuation),
        asTriple(oTypes.QuadraticAttenuation, spotLight.setQuadraticAttenuation( _ : Float ), spotLight.getQuadraticAttenuation),
        asTriple(oTypes.SpotCutOff, spotLight.setSpotCutOff( _ : Float ), spotLight.getSpotCutOff),
        asTriple(oTypes.SpotExponent, spotLight.setSpotExponent( _ : Float ), spotLight.getSpotExponent),
        asTriple(oTypes.CastShadow, spotLight.setCastShadow( _ : Boolean ), spotLight.isCastingShadow),
        asTriple(oTypes.ShadowBias, spotLight.setShadowBias( _ : Float ), spotLight.getShadowBias)
      )
    }

    sender ! ElementInjected( entity, aspect )
  }

  private def createPointLight( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect, fresh : SValSet ) {
    def fget[T]( c : ConvertibleTrait[T] ) : T  = fresh.firstValueFor(c)

    val name = aspect.getCreateParams.firstValueFor( oTypes.Name )
    val transformation = fget( types.Transformation )
    val parentElement = aspect.getCreateParams.getFirstValueFor( oTypes.ParentElement )

    val pointLight = new PointLightNode( name )
    pointLight.setTransform( transformation )
    pointLight.setDiffuseColor(         fget( types.DiffuseColor ) )
    pointLight.setSpecularColor(        fget( types.SpecularColor ) )
    pointLight.setLinearAttenuation(    fget( oTypes.LinearAttenuation ) )
    pointLight.setConstantAttenuation(  fget( oTypes.ConstantAttenuation ) )
    pointLight.setQuadraticAttenuation( fget( oTypes.QuadraticAttenuation ) )

    addInsertion(entity){ e : Entity =>
      val hullNode = insertNode (e, pointLight, transformation, parentElement )
      connect(e,
        asTriple(types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform),
        asTriple(types.DiffuseColor , pointLight.setDiffuseColor( _ : Color), pointLight.getDiffuseColor),
        asTriple(types.SpecularColor, pointLight.setSpecularColor( _ : Color), pointLight.getSpecularColor),
        asTriple(oTypes.LinearAttenuation, pointLight.setLinearAttenuation( _ : Float ), pointLight.getLinearAttenuation),
        asTriple(oTypes.ConstantAttenuation, pointLight.setConstantAttenuation( _ : Float ), pointLight.getConstantAttenuation),
        asTriple(oTypes.QuadraticAttenuation, pointLight.setQuadraticAttenuation( _ : Float ), pointLight.getQuadraticAttenuation)
      )
    }
    sender ! ElementInjected( entity, aspect )
  }


  private def createSkyBox( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect ) {
    def aget[T]( c : ConvertibleTrait[T] ) = aspect.getCreateParams.firstValueFor(c)

    val creater = SkyBoxCreator(
      name          = aget( oTypes.Name ),
      backTexture   = aget( oTypes.BackTexture  ),
      bottomTexture = aget( oTypes.DownTexture ),
      frontTexture  = aget( oTypes.FrontTexture ),
      leftTexture   = aget( oTypes.LeftTexture ),
      rightTexture  = aget( oTypes.RightTexture ),
      topTexture    = aget( oTypes.UpTexture )
    )

    for( (_,c ) <- cameras ) {
      for( (_,camera) <- c ) {
        val skyBox = creater.create( camera.getName )
        sceneRoot.addChildNode( skyBox )
        skyBoxes = skyBoxes + (camera -> skyBox)
      }
    }

    for( w <- windows ) {
      for( (_,mc ) <- mirrorCameras(w) ) {
        for( (_,camera) <- mc ) {
          val skyBox = creater.create( camera.getName )
          sceneRoot.addChildNode( skyBox )
          skyBoxes = skyBoxes + (camera -> skyBox)
        }
      }
    }
    sender ! ElementInjected( entity, aspect )
  }

  private def createShapeFromFile( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect, fresh : SValSet ) {
    def aget[T]( c : ConvertibleTrait[T] ) = aspect.getCreateParams.getFirstValueFor(c)

    val fileName        = aget( oTypes.ColladaFile ).get
    val subElement      = aget( oTypes.SubElement )
    val parentElement   = aget( oTypes.ParentElement )
    val transformation  = fresh.firstValueFor( types.Transformation )
    val scale           = aget( types.Scale )

    ask( ResourceManager.self, AskColladaFile(new File(fileName))  ){
      source : GroupNode =>
        val shape = subElement.collect{ case name => Finder.find( source, classOf[GroupNode], name ) }.getOrElse(source)
        val shaderEffect = aspect.getCreateParams.getFirstValueFor( types.ShaderEffect )

        entityToDirectParentOfShape = entityToDirectParentOfShape.updated(entity, shape)

        shaderEffect.collect {
          case se =>

            val node = Finder.find( shape, classOf[ShapeNode], null )
            node.setMaterial( se.getShaderMaterial )
            shaderEffectMap = shaderEffectMap.updated(node, se)
        }

        addInsertion(entity){ e : Entity =>
          val hullNode = insertNode(e, shape, transformation, parentElement, scale)
          connect(e, asTriple(types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform))
          shaderEffect.collect{ case se => se.bindShaderMaterialToEntity(e) }
        }

        sender ! ElementInjected( entity, aspect )
    }
  }

  //private def createAnimationObject( entity : Entity, aspect : EntityAspect ) = null
  private def createMirrorLike(sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect, fresh : SValSet,
                               transformation : Transform, fileName : String, shaderMat : ShaderMaterial){
    val name = aspect.getCreateParams.firstValueFor( oTypes.Name )
    val hullNode = new GroupNode( name + "_hull")
    val clipPlane = new ClipPlaneNode
    ask(ResourceManager.self, AskColladaFile( new File( fileName ) )){
      shape : GroupNode =>
        clipPlane.setTransform( Transform.rotateX( functions.radians( 180f ) ) )
        Finder.find(shape, classOf[ShapeNode], null).setMaterial(shaderMat)
        hullNode.addChildNode( clipPlane )
        hullNode.addChildNode( shape )
        hullNode.setTransform( transformation )
        sceneRoot.addChildNode( hullNode )
        entityToNodeMap = entityToNodeMap.updated(entity, hullNode :: entityToNodeMap.getOrElse(entity, Nil) )

        for( (window,c) <- camList ) {
          for( cam <- c ) {
            val mirrorCamera = new VRCameraNode(
              cam.getName + "_mirror_" + name,
              cam.getScreenTransform,
              new Vector4( cam.getScreenDim.y, cam.getScreenDim.x, cam.getScreenDim.z, cam.getScreenDim.w ),
              cam.isLeftEye,
              cam.getTransform
            )
            cameraUpdater = cameraUpdater ::: new MirrorCameraUpdater( mirrorCamera, cam, hullNode ) :: Nil
            var map = mirrorCameras(window).get( cam ) match {
              case Some( m ) => m
              case None => Map[Symbol,VRCameraNode]()
            }
            map = map + ( Symbol( name )  -> mirrorCamera)

            mirrorCameras = mirrorCameras + (window -> (mirrorCameras(window) + (cam  -> map)))

            skyBoxCreator match {
              case Some( s ) =>
                val skyBox = s.create( mirrorCamera.getName )
                sceneRoot.addChildNode( skyBox )
                skyBoxes = skyBoxes + (mirrorCamera -> skyBox)

              case None =>
            }
          }
        }
        effectplanes = effectplanes ::: name :: Nil

        refreshPipelines()

        connect(entity, asTriple(types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform))
    }
  }

  private def createMirror( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect, fresh : SValSet ) {
    val fileName = aspect.getCreateParams.firstValueFor( oTypes.ColladaFile )
    val name = aspect.getCreateParams.firstValueFor( oTypes.Name )
    ask(ResourceManager.self, AskShaderProgram( new File("shader/mirror.vs") :: new File("shader/mirror.fs") :: Nil ) ) {
      prog : ShaderProgram =>
        val mirrorMat = new ShaderMaterial( name + "_pass", prog )
        addInsertion(entity){ e : Entity =>
          createMirrorLike(sender, e, aspect, fresh, Transform.identity(), fileName, mirrorMat )
        }
        sender ! ElementInjected( entity, aspect )
    }
  }

  private def createWater( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect, fresh : SValSet ) {
    val name = aspect.getCreateParams.firstValueFor( oTypes.Name )
    val transformation = fresh.firstValueFor(types.Transformation)
    ask(ResourceManager.self, AskShaderProgram( new File("shader/mirror.vs") :: new File("shader/mirror.fs") :: Nil ) ){
      prog : ShaderProgram =>
        val waterMat = new ShaderMaterial( name + "_pass", prog )
        val waveScaleValue = fresh.firstValueFor( oTypes.WaveScale )

        waterMat.setUniform( name + "_pass", "waveTime", new UniformFloat( (System.nanoTime -t0) * 1e-4f ))
        waterMat.setUniform( name + "_pass","waveScale", new UniformFloat(waveScaleValue))
        waterMats = waterMats + (name -> waterMat)
        addInsertion(entity) { e : Entity =>
          createMirrorLike(sender, e, aspect, fresh, transformation, "models/plane.dae", waterMat)
          connect(e, asTriple(oTypes.WaveScale, (ws: Float) => waterMat.setUniform(name + "_pass","waveScale", new UniformFloat(ws)), () => 0f))
        }
        sender ! ElementInjected( entity, aspect )
    }
  }

  private def createExistingNode( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect ) {
    val subElement = aspect.getCreateParams.firstValueFor( oTypes.SubElement )
    val n : SceneNode = Finder.find( sceneRoot, classOf[SceneNode], subElement )
    val worldTransform = n.getWorldTransform( sceneRoot )
    val scale = aspect.getCreateParams.firstValueFor(types.Scale)
    val shaderEffects = aspect.getCreateParams.getAllValuesFor( types.ShaderEffect )

    if( !shaderEffects.isEmpty ) {
      val shaderEffect = aspect.getCreateParams.firstValueFor( types.ShaderEffect )
      val shape = Finder.find( n, classOf[ShapeNode], null )
      shape.setMaterial( shaderEffect.getShaderMaterial )
    }

    //extractScale
    val rot = worldTransform.extractRotation()
    val s   = rot.getMatrix.mul(rot.getMatrix.transpose())
    val innerScale = Transform.scale(math.sqrt(s.get(0,0)).toFloat, math.sqrt(s.get(1,1)).toFloat, math.sqrt(s.get(2,2)).toFloat)
    val innerTranslation = Transform.translate(worldTransform.extractTranslation().getMatrix.translation())

    n.getParentNode.asInstanceOf[GroupNode].removeChildNode( n )
    addInsertion(entity){ e : Entity =>
      val hullNode = insertNode(e, n, innerTranslation.mul(innerScale.invert().mul(rot)), None, Some(scale.mul(innerScale)))
      singleConnect[Transform](e, (types.Transformation, (in : Transform ) => hullNode.setTransform( in ), hullNode.getTransform), useGet = true)
      shaderEffects.foreach(_.bindShaderMaterialToEntity(e))
    }
    sender ! ElementInjected( entity, aspect )
  }

  private def refreshPipelines(){
    for( (window,pipeline) <- pipelines ) {
      pipeline.clearPipeline()
      createDefaultPipeline( pipeline, cameras(window), sceneRoot, window )
    }
  }

  private def addPostProcessingEffect( e : Entity, ppe : PostProcessingEffect* ){
    addInsertion(e){ (e:Entity) =>
      postProcessingEffects = postProcessingEffects ::: ppe.toList
      entityToPostProcessingEffects = entityToPostProcessingEffects + (e -> ppe.toList)
      refreshPipelines()
      bindPostProcessingEffectToEntity( ppe.head, e )
    }

  }


  private def bindPostProcessingEffectToEntity( ppe : PostProcessingEffect, entity : Entity ) {
    def setInitialValue[T, U](um : UniformManager[T, U, _]){
      entity.get(um.ontologyMember.get).head.set( um.converter._2(um.value) )
    }

    for( uniformManager <- ppe.uniformList ) {
      if( uniformManager.ontologyMember.isDefined ){
        setInitialValue(uniformManager)
        uniformManager.value match {
          case v : Texture2D =>
            entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[TextureData]].observe{
              tex => ppe.getShaderMaterial.setTexture(ppe.nameOfEffect.get, uniformManager.name, new Texture2D(tex.size.x, tex.size.y, tex.data))
            }
          case v : Float =>
//            set( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Float]], v )
            observe( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Float]] )( ( v : Float ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformFloat( v ) ) } )
          case v : Int =>
//            set( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Int]], v )
            observe( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Int]])( ( v : Int ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformFloat( v ) ) } )
          case v : Boolean =>
//            set( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Boolean]], v )
            observe( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Boolean]])( ( v : Boolean ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformBool( v ) ) } )
          case v : Vector2 =>
//            set( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Vector2]], v )
            observe( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[Vector2]])( ( v : Vector2 ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector2( v ) ) } )
          case v : ConstVec4f =>
//            set( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[ConstVec4f]], v )
            observe( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[ConstVec4f]])( ( v : ConstVec4f ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector4( new Vector4(v.x, v.y, v.z, v.w) ) ) } )
          case v : List[_] =>
//            set( entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[List[Vec2f]]], v.asInstanceOf[List[Vec2f]] )
            entity.get( uniformManager.ontologyMember.get ).head.asInstanceOf[SVar[List[Vec2f]]].observe{ ( list : List[Vec2f] ) =>
              ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name + "_size", new UniformInt( list.size ) )
              if( list.isEmpty ) {
                ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector2( new Vector2( 0.0f, 0.0f ) ) )
              } else {
                val jvrList = list.map(s3dVec => {new Vector2(s3dVec.x, s3dVec.y)})
                ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector2( jvrList.toArray : _* ) )
              }
            }
        }
      }
    }
  }

  private def createPostProcessingEffect( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect ) {
    val ppe = aspect.getCreateParams.getFirstValueFor( simx.components.renderer.jvr.types.PostProcessingEffect ).get
    addPostProcessingEffect( entity, ppe )
    sender ! ElementInjected( entity, aspect )
  }

  var remoteStepCount : Option[Long] = None

  addHandler[PrintStepCountOf] {
    msg =>
      msg.source ! ObserveStepCount()
  }

  addHandler[CurrentStepCount] {
    msg =>
      remoteStepCount = Some( msg.stepCount )

  }

}

object StereoMode extends Enumeration {

  val AnaglyphStereo = Value( "AnaglyphStereo" )

  val FrameSequential = Value( "FrameSequential" )

  val TopBottomStereo = Value( "TopBottomStereo" )

}