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

import java.io.File
import javax.media.opengl.GL2GL3

import de.bht.jvr.input.{MouseEvent, MouseListener, KeyEvent, KeyListener}
import de.bht.jvr.math.{Vector2, Vector4}
import de.bht.jvr.core._
import de.bht.jvr.renderer._
import simx.core.ontology.types.PartOf
import simx.core.svaractor.unifiedaccess.{Remove, Update, Add, ?}
import uniforms._
import pipeline.{PipelineCommandPtr, Pipeline}

import simx.core.component._
import simx.core.components.io.IODeviceProvider
import simx.core.entity.Entity
import simx.core.entity.description._
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}
import simx.core.helper.{TextureData, SVarUpdateFunctionMap}
import simx.core.ontology.{types => oTypes, SVarDescription, Symbols}
import simx.core.svaractor.{StateParticle, SVar, SVarActor}
import simx.core.worldinterface.eventhandling.EventProvider

import simplex3d.math.floatx.{Vec3f, Vec2f, ConstVec3f, ConstVec4f, functions}

import scala.reflect.ClassTag
import de.bht.jvr.util.Color
import scala.collection.immutable.HashSet
import simplex3d.math.ConstVec2i



/**
 * A JVRRenderActor manages a scene graph, constructs a rendering pipeline and renders to one or more windows.
 *
 * It is started by the [[simx.components.renderer.jvr.JVRConnector]] depending on the passed DisplayDescription.
 *
 * @author Stephan Rehfeld
 */
class JVRRenderActor extends SVarActor with SVarUpdateFunctionMap with IODeviceProvider with EventProvider {

  protected def removeFromLocalRep(e : Entity){
    println(this + " should remove " + e)
  }

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
  private val mouseMappings = SValSet()

  private var mouse : Map[Int, Entity] = Map()

  private var keyboard : Map[Int, Entity] = Map()

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

  addHandler[DetachObject] {
    msg => entityToNodeMap.get(msg.e).collect {
      case nodeList => nodeList.foreach( e => e.getParentNode.asInstanceOf[GroupNode].removeChildNode(e) )
    }.getOrElse(warn("detaching failed: could not find " + msg.e.getSimpleName + " in entityToNodeMap"))
  }

  addHandler[AttachObject]{
    msg => entityToNodeMap.get( msg.e ).collect{
      case nodeList => nodeList.foreach(setParent(_, sceneRoot))
    }.getOrElse(warn("attaching failed: could not find " + msg.e.getSimpleName + " in entityToNodeMap"))
  }

  addHandler[RegroupEntity] { msg =>
    val parent = msg.target.collect{
      case target if entityToNodeMap contains target => entityToNodeMap(target).head
      case target =>
        warn("regrouping failed: could not find " + target.getSimpleName + " in entityToNodeMap, regrouping to sceneRoot instead")
        sceneRoot
    }.getOrElse(sceneRoot)
    regroup(msg.e, parent, msg.convertTransform)
    msg.sender ! RegroupApplied(msg.e, msg.target)
  }
                    //child,              parent
  private def regroup(toRegroup : Entity, target : GroupNode, convertTransform : Boolean = true) {
    println("regrouping " + toRegroup.getSimpleName + " under " + target.getName)
    entityToNodeMap.get(toRegroup)map(_.foreach { x =>
      regroup(x, target, convertTransform).collect {
        case newTransformation =>
          toRegroup.set(oTypes.Transformation(JVRConnector.transformConverter.convert(newTransformation)))
      }
    })
  }

  def areEqual(t1: Transform, t2: Transform) =
    t1.getMatrix.equals(t2.getMatrix)

  private def regroup(toRegroup : SceneNode, target : GroupNode, convertTransform : Boolean) = {
    var retVal : Option[Transform] = None
    if (convertTransform) {
      val worldTransform = toRegroup.getWorldTransform(sceneRoot)
      val worldTransformOfTarget = target.getWorldTransform(sceneRoot)
      val newTransform = worldTransformOfTarget.invert.mul(worldTransform)
      if(!areEqual(worldTransform, newTransform)) {
        toRegroup.setTransform(newTransform)
        retVal = Some(newTransform)
      }
    }
    if (toRegroup.getParentNode != null)
      toRegroup.getParentNode.asInstanceOf[GroupNode].removeChildNode( toRegroup )
    setParent( toRegroup, target )
    retVal
  }

  /**
   * This flag shows if the user entity has already been registered.
   */
  private var userEntityIsRegistered = false

  addHandler[JVRPublishUserEntity] {
    msg: JVRPublishUserEntity =>
      if(!userEntityIsRegistered){
        val viewPlatform = msg.user.getSVars(ontology.types.ViewPlatform).head._2
        val headTransform = msg.user.getSVars(ontology.types.HeadTransform).head._2

        viewPlatform.observe( setTransform _)
        viewPlatform.get( setTransform _)
        headTransform.observe( setHeadTransform _)
        headTransform.get( setHeadTransform _)
        userEntityIsRegistered = true
      }
  }

  addHandler[CreateMesh]{ msg =>
    createMesh(msg.e, msg.asp, msg.given)
    sender ! MeshCreated(msg.e, msg.ready)
  }

  addHandler[RequestTransformation]{
    msg =>
      val n : SceneNode = Finder.find( sceneRoot, classOf[SceneNode], msg.element )
      val worldTransform = n.getWorldTransform( sceneRoot )
      msg.sender ! TellTransformation( msg.entity, worldTransform, msg.aspect )
  }

  private def clearSettings(){
    winId = 0
    windowsToClose += 1
    viewer.collect { case v => v.close();}
    viewer = None
    windows.foreach( x => windowSettings = windowSettings - x )
    windows = Nil
    pipelines = pipelines.empty
    mirrorCameras = mirrorCameras.empty
    cameras.values.foreach(_.values.foreach(sceneRoot.removeChildNode))
    cameras = cameras.empty
    camList = camList.empty
    stereoMode = None
  }


  private var windowSettings = Map[RenderWindow, RenderActorConfig]()

  private def createWindow(config : RenderActorConfig, pipeline : Pipeline) = {
    val window = config.resolution match {
      case Some((width, height)) =>
        new NewtRenderWindow(pipeline, width, height)
      case None =>
        //if(JVMTools.isWindows) new NewtRenderWindow( pipeline, true )
        //    else
        new NewtRenderWindow(pipeline, true)
    }
    config.hardwareHandle.collect{ case (_, screen, _) => window.setScreenDevice( screen ) }
    window.setVSync( true )

    config.stereoMode match {
      case Some( StereoMode.FrameSequential ) =>
        window.setStereo( true )
      case Some(_)  =>
      case None     => window.setStereo( false )
    }
    windowSettings = windowSettings.updated(window, config)
    window
  }

  private def setupWindow( window : RenderWindow, config : RenderActorConfig ) = {
    if (config.resolution.nonEmpty)
      window.setSize(config.resolution.get._1, config.resolution.get._2)

    window
  }

  private def createCam(c : RenderActorConfig, isLeft : Option[Boolean]) : VRCameraNode = {
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

  private def setupCameras(config : RenderActorConfig, window : RenderWindow){
    mirrorCameras = mirrorCameras + (window -> Map())
    cameras       = cameras + (window -> Map())
    camList       = camList + (window -> List())

    if( config.eyeToRender == EyeToRender.Both ) {
      cameras = cameras + (window -> ( cameras( window ) + ('standardLeft  -> createCam(config, Some(true )) ) ) )
      cameras = cameras + (window -> ( cameras( window ) + ('standardRight -> createCam(config, Some(false)) ) ) )
    } else {
      cameras = cameras + (window -> ( cameras( window ) + ('standard -> createCam(config, None) ) ) )
    }
    if( cameras( window ).contains( 'standard ) ) {
      setParent( cameras( window )( 'standard ), sceneRoot )
      camList = camList + (window -> (camList(window) ::: cameras( window )( 'standard ) :: Nil))
    } else {
      setParent( cameras( window )( 'standardLeft ), sceneRoot )
      setParent( cameras( window )( 'standardRight ), sceneRoot )
      camList = camList + (window -> (camList(window) ::: cameras( window )( 'standardLeft ) :: Nil))
      camList = camList + (window -> (camList(window) ::: cameras( window )( 'standardRight ) :: Nil))
      stereoMode = config.stereoMode
    }
  }

  private def configureInputDevices(config : RenderActorConfig, window : RenderWindow, winId : Int){
    def nameIt( n : String, e : Entity )(handler : Entity => Any) =
      e.set(oTypes.Name.apply(n + " (" + id + ", " + winId + ")"), handler)

    mouseMappings add oTypes.Origin(Vec3f.Zero)
    mouseMappings add oTypes.Direction(Vec3f.Zero)
    mouseMappings add oTypes.Position2D(Vec2f.Zero)
    mouseMappings add oTypes.Button_Left(false)
    mouseMappings add oTypes.Button_Right(false)
    mouseMappings add oTypes.Button_Center(false)

    //def recurseBtns(e : Entity, remaining : List[(SVal[X, TypeInfo[X, X]]) forSome {type X}]) {
    def recurseBtns(e : Entity, remaining : List[SVal.SValType[_]]) {
      remaining match {
        case head :: tail => e.set(head, recurseBtns(_ : Entity, tail))
        case Nil => nameIt("Mouse", e){ mouseEntity =>
          mouse  = mouse.updated( winId, mouseEntity )
          publishDevice( oTypes.Mouse( mouseEntity ), Symbol(config.hardwareHandle.getOrElse((0,0,0))._2.toString) :: Nil )
        }
      }
    }

    def recurseKeys(e : Entity, remaining : List[(Int, SVarDescription[Boolean, Boolean])]) {
      remaining match {
        case head :: tail =>
          e.set( head._2(false), { next : Entity =>
            keyboardMap.update(head._1, next.getSVars(head._2).head._2.asInstanceOf[SVar[Boolean]].set( _ ) )
            recurseKeys(next, tail)
          })

        case Nil => nameIt("Keyboard", e){
          kb =>
            keyboard = keyboard.updated(winId, kb)
            publishDevice( oTypes.Keyboard(kb),  Symbol(config.hardwareHandle.getOrElse((0,0,0))._2.toString) :: Nil)
        }
      }
    }

    recurseKeys(keyboard.getOrElse(winId, new Entity), oTypes.Mappings.keyMap.toList)
    recurseBtns(mouse.getOrElse(winId, new Entity), mouseMappings.toSValSeq.toList)
    registerListeners(window, config, winId)
  }

  private var winId : Int = 0

  private def settingsMatch(window : RenderWindow, settings : RenderActorConfig) = {
    windowSettings get window match {
      case Some(winSettings)  =>
        settings.stereoMode.equals(winSettings.stereoMode) &&
          settings.hardwareHandle.equals(winSettings.hardwareHandle) &&
          winSettings.resolution.isDefined == settings.resolution.isDefined
      case None => false
    }
  }

  addHandler[RenderActorConfigs] {
    configs : RenderActorConfigs =>

      frequency = configs.frequency
      shadows = !configs.shadowQuality.equals( "none" )
      shadowQuality = configs.shadowQuality
      mirrorQuality = configs.mirrorQuality

      if (configs.configs.size != windows.size || configs.configs.zip(windows).exists( t => !settingsMatch(t._2, t._1)))
        clearSettings()

      var remainingWindows = windows
      for( config <- configs.configs) {

        val pipeline = remainingWindows.headOption.collect{
          case win if pipelines.contains(win) && settingsMatch(win, config) => pipelines(win)
        }.getOrElse( new Pipeline( sceneRoot ))

        val window = remainingWindows.headOption match {
          case Some(win) if settingsMatch(win, config) => win
          case _ => createWindow(config, pipeline)
        }

        setupWindow(window, config)
        setupCameras(config, window)

        pipeline.clearPipeline()
        createDefaultPipeline( pipeline, cameras( window ), sceneRoot, window )

        pipelines  = pipelines + (window -> pipeline )

        if (!windows.contains(window)) {
          configureInputDevices(config, window, winId)
          windows = windows ::: window :: Nil
        }

        if (remainingWindows.nonEmpty)
          remainingWindows = remainingWindows.tail

        winId += 1
      }

      windowsToClose = windows.size
      if (viewer.isEmpty)
        viewer = Some(new Viewer(true, windows: _*))

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
      try {
        viewer.collect { case v => v.close() }
      } catch {
        case e : Exception =>
      }
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
          setParent(cam, g)
          setParent(g, node.head)
        }
      })
    })
  }

  private var windowsToClose = 0

  /**
   * This helper function registers listener to the windows of this render actor to provide the input of mouse and
   * keyboard.
   *
   * @param window The windows.
   * @param config The configuration of the render window.
   */
  private def registerListeners( window : RenderWindow, config : RenderActorConfig, winId : Int ) {
    require( window != null, "The parameter 'window' must not be 'null'!" )
    require( config != null, "The parameter 'config' must not be 'null'!" )

    jvrConnector = Some( config.sender )

    window.addWindowListener{
      new WindowListener {
        override def windowReshape(p1: RenderWindow, p2: Int, p3: Int) {}
        override def windowClose(p1: RenderWindow) {
          println("window closed")
          windowsToClose -= 1
          if (windowsToClose == 0)
            me.self ! JVRRenderWindowClosed()
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
      def sett[T : ClassTag]( sval : SVal[T,TypeInfo[T,T]] ){
        if(mouseMappings.contains(sval.typedSemantics)) mouse.get(winId).collect{case m => m.set( sval )
          //        mouseMappings.find( _.typedSemantics == sval.typedSemantics).collect{
          //          case t => mouse.get(winId).collect{case m => m.set( sval ) }
        }
      }

      var isPressed = -1

      def mouseReleased(e: MouseEvent) {
        buttons.get(e.getButton).collect{ case x => sett(x(false)) }
        isPressed = -1
        updatePickRay(e, isPressed)
      }

      def mousePressed(e: MouseEvent) {
        buttons.get(e.getButton).collect{ case x => sett(x(true)) }
        isPressed = 1
        updatePickRay(e, isPressed)
      }

      def mouseMoved( e: MouseEvent ) {
        sett( oTypes.Position2D(Vec2f( e.getX.toFloat, e.getY.toFloat )) )
        updatePickRay(e, isPressed, sendEvent = false)
      }

      def mouseClicked(e: MouseEvent) {

      }

      def mouseExited(e: MouseEvent)    {}
      def mouseEntered(e: MouseEvent)   {}
      def mouseDragged(e: MouseEvent)   {sett( oTypes.Position2D(Vec2f( e.getX.toFloat, e.getY.toFloat )) )}
      def mouseWheelMoved(e: MouseEvent){}

      private def updatePickRay(e: MouseEvent, state : Int, sendEvent : Boolean = true) {
        val pickRay = Picker.getPickRay(sceneRoot, cameras.head._2.head._2, e.getNormalizedX, e.getNormalizedY)
        sett(oTypes.Origin(ConstVec3f(pickRay.getRayOrigin.x, pickRay.getRayOrigin.y, pickRay.getRayOrigin.z)))
        sett(oTypes.Direction(ConstVec3f(pickRay.getRayDirection.x, pickRay.getRayDirection.y,pickRay.getRayDirection.z)))
        if (sendEvent)
          self ! RequestPick(pickRay, state)
      }
    })
  }

  private case class RequestPick(pickRay : PickRay, state : Int)
  addHandler[RequestPick]{
    msg => pick(msg.pickRay).collect{ case entity => JVRPickEvent.emit(
      simx.core.ontology.types.Entity(entity),
      simx.core.ontology.types.Enabled(msg.state >= 0)
    ) }
  }

  private def pick(ray : PickRay) : Option[Entity] = {
    import scala.collection.JavaConversions._

    def findEntity(nodes: List[SceneNode]): Option[Entity] = nodes match {
      case Nil => None
      case head :: tail =>
        entityToNodeMap find (_._2.contains(head)) match {
          case None => findEntity(tail ::: head.getAllParentNodes.toList)
          case Some((entity, _)) => Some(entity)
        }
    }

    Picker.pickShapeNode(sceneRoot, ray, null).getShapeNode match {
      case null => None
      case node => findEntity(node :: Nil)
    }
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

      viewer.collect{ case v if windowsToClose > 0 => v.display() }
      jvrConnector.collect{ case connector => connector ! JVRFrameFinished(self, System.nanoTime()) }
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
        case Symbols.`meshComponent`      => insertMesh(msg.sender, msg.e, msg.aspect)
        case Symbols.parentElement        => createGroupNode(msg.sender, msg.e, msg.aspect)
        case x => println(this + " ignoring aspect " +  x)
      }
  }

  addHandler[RemoveSceneElement] {
    msg =>
      entityToNodeMap.get(msg.e) match {
        case Some(nodes) =>
          nodes.foreach(node => node.getParentNode match {
            case parent: GroupNode =>
              parent.removeChildNode(node)
          })
          entityToNodeMap -= msg.e
        case None =>
          println("tried to remove non-existent entity " + msg.e)
      }
      msg.e.getAllStateParticles.foreach( triple => ignoreMultiobserve(triple.svar) )
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

  private type ConnectTriple[T] = (ConvertibleTrait[T], T => Unit, () => T)
  private def connect( e : Entity, list : ConnectTriple[_]* ) { list.foreach( triple => singleConnect(e, triple, useGet = true ) ) }

  private def singleConnect[T: ClassTag]( entity : Entity, triple : ConnectTriple[T], useGet : Boolean = false ){
    entity.getSVars(triple._1).headOption match {
      case Some(svar) => addMultiobserve(svar._2, useGet)(triple._2)
      case None       => println("error, could not get " + triple._1)
    }
  }

  def createGroupNode( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect ){
    val parentNode = if (entityToNodeMap.contains(entity))
      entityToNodeMap(entity).head
    else {
      val pNode = new GroupNode(entity.getSimpleName)
      entityToNodeMap = entityToNodeMap.updated(entity, pNode :: Nil)
      pNode
    }
    addInsertion(entity) { e =>
      insertNode(e, parentNode, new Transform, None, None)
      connect(e,
        asTriple(ontology.types.Transformation, (in: Transform) =>  parentNode.setTransform(in), parentNode.getTransform)
      )
    }
    sender ! ElementInjected(entity, aspect)
  }

  val placeHolderSufix = "_hull-placeholder"

  private def setParent(child : Entity, parent : Entity){
    entityToNodeMap.get(parent) match {
      case Some(parentNode :: _) =>
        val regroupingFromPlaceholderParent =
          entityToNodeMap.get(child).exists(_.head.getParentNode.getName.contains(placeHolderSufix))
        regroup(child, parentNode, !regroupingFromPlaceholderParent)
      case None =>
        val parentNode = new GroupNode(parent.getSimpleName + placeHolderSufix) // will be replaced later
        entityToNodeMap = entityToNodeMap.updated(parent, parentNode :: Nil)
        regroup(child, parentNode, convertTransform = false)
        if(!parent.description.aspects.exists(_.componentType == Symbols.graphics)) {
          parent.get(ontology.types.Transformation).ifEmpty {
            regroup(parentNode, sceneRoot, convertTransform = false)
          }.head { transformation =>
            regroup(parentNode, sceneRoot, convertTransform = true)
            parent.observe(simx.components.renderer.jvr.ontology.types.Transformation).head{ newValue =>
              parentNode.setTransform(newValue)
            }
          }
        }
      case _ =>
        throw new Exception("unexpected configuration")
    }
  }

  def setParent(child : Entity, parent : GroupNode){
    if (entityToNodeMap.contains(child))
      setParent(entityToNodeMap(child).head, parent)
  }

  private def setParent(child : GroupNode, parent : Entity){
    if (entityToNodeMap.contains(parent))
      setParent(child, entityToNodeMap(parent).head)
  }

  private def setParent(child : SceneNode, parent : GroupNode){
    if (!parent.getChildNodes.contains(child))
      parent.addChildNode(child)
  }

  private def insertNode( e : Entity, toInsert : SceneNode, trafo : Transform,
                          parentElement : Option[Entity], scale : Option[Transform] = None ) = {
    val hullNode = new GroupNode( e.getSimpleName + "_hull")
    hullNode.setTransform( trafo )
    if (scale.isDefined){
      val scaleNode = new GroupNode( e.getSimpleName + "_scale")
      scaleNode.setTransform(scale.get)
      setParent(toInsert, scaleNode)
      setParent(scaleNode, hullNode)
    }
    else setParent( toInsert, hullNode )

    toInsert.setTransform( new Transform )

    //replace hullNode with its placeholder(s)
    var replacedNodes = List[SceneNode]()
    if(entityToNodeMap.contains(e)) {
      val placeHolderNodes = entityToNodeMap.get(e)
      placeHolderNodes.map(_.foreach {
        case g: GroupNode =>
          replacedNodes ::= g
          val it = g.getChildNodes.listIterator()
          var children = List[SceneNode]()
          while (it.hasNext) {children ::= it.next()}
          children.foreach(regroup(_, hullNode, convertTransform = false))
      })
    }

    entityToNodeMap =
      entityToNodeMap.updated(e, hullNode :: entityToNodeMap.getOrElse(e, Nil).filterNot(replacedNodes.contains))

    e.observe(PartOf -> ?).onChange{
      case Add(_, parent) =>
        setParent(e, parent)
      case Update(_, parent) =>
        setParent(e, parent)
      case Remove(_, x) =>
        regroup(e, sceneRoot)
    }

    parentElement match {
      case Some( gn ) => setParent(hullNode, gn )
      case None => regroup( hullNode, sceneRoot, convertTransform = true )
    }

    e.getSVars(simx.core.ontology.types.Texture).collect{
      case textureSVar => entityToDirectParentOfShape.get(e).collect{
        case parentOfShapeNode if parentOfShapeNode.getChildNodes.size == 1 =>
          parentOfShapeNode.getChildNodes.get(0) match {
            case child: GroupNode if child.getNumParentNodes > 1 =>
              parentOfShapeNode.removeChildNode(child)
              setParent(cloneSubTree(e, child), parentOfShapeNode)
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

          var textures = HashSet[Texture2D]()
          shapeNode.getMaterial match {
            case sm: ShaderMaterial =>
              forall(sm.getShaderContexts.values())(shaderCtx => {
                forall(shaderCtx.getTextures.values()) {
                  case tex2D: Texture2D => textures += tex2D
                  case _ =>
                }
              })
            case _ =>
          }

          textureSVar._2.get(tex => {
            if(textures.nonEmpty && tex.size == JVRConnector.voidTextureData.size && tex.data == JVRConnector.voidTextureData.data) {
              val t = textures.head
              //TODO: Correct creation for non-power-of-two-sized images. See de.bht.jvr.core.Texture2d.load(Raster)
              textureSVar._2.set(TextureData(ConstVec2i(t.getWidth,t.getHeight),t.getImageData))
            }

            textureSVar._2.observe(handleTexUpdate _)
            textureSVar._2.get(handleTexUpdate _)
          })


      }
    }

    hullNode
  }

  private def forall[T](c: java.util.Collection[T])(handler: T => Unit) {
    val it = c.iterator()
    while(it.hasNext) handler(it.next())
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
      case g: GroupNode => setParent(cloneSubTree(e, g), result)
      case p : LightNode => setParent(p.getRenderClone, result)
      case s: ShapeNode => setParent(cloneShapeNode(e, s), result)
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
        msg.toInsert.getSVars(oTypes.Name).headOption match {
          case Some((_, svar)) =>
            get( svar)( (name : String) => info("inserting " + name + " into scenegraph") )
            toInsert = toInsert - msg.toInsert
            funcList.foreach( _.apply(msg.toInsert) )
          case None       =>
            info("inserting entity " + msg.toInsert + " into scenegraph" )
            toInsert = toInsert - msg.toInsert
            funcList.foreach( _.apply(msg.toInsert) )
        }
      case None => msg.toInsert.getSVars(oTypes.Name).headOption match {
        case Some((_, svar)) => get( svar )( name => info("entity " + name + " could not be inserted since it was not found"))
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
    val transformation = fget( ontology.types.Transformation )

    val spotLight = new SpotLightNode( aspect.getCreateParams.firstValueFor( oTypes.Name ) )
    spotLight.setTransform(            transformation )
    spotLight.setCastShadow(           fget( oTypes.CastShadow ) )
    spotLight.setShadowBias(           fget( oTypes.ShadowBias ) )
    spotLight.setSpotCutOff(           fget( oTypes.SpotCutOff ) )
    spotLight.setSpotExponent(         fget( oTypes.SpotExponent ) )
    spotLight.setDiffuseColor(         fget( ontology.types.DiffuseColor  ) )
    spotLight.setSpecularColor(        fget( ontology.types.SpecularColor ) )
    spotLight.setLinearAttenuation(    fget( oTypes.LinearAttenuation ) )
    spotLight.setConstantAttenuation(  fget( oTypes.ConstantAttenuation ) )
    spotLight.setQuadraticAttenuation( fget( oTypes.QuadraticAttenuation ) )

    addInsertion(entity){ e : Entity =>
      val hullNode = insertNode(e, spotLight, transformation, parentElement)
      connect(e,
        asTriple(ontology.types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform),
        asTriple(ontology.types.DiffuseColor, spotLight.setDiffuseColor( _ : Color ), spotLight.getDiffuseColor),
        asTriple(ontology.types.SpecularColor, spotLight.setSpecularColor( _ : Color ), spotLight.getSpecularColor),
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
    val transformation = fget( ontology.types.Transformation )
    val parentElement = aspect.getCreateParams.getFirstValueFor( oTypes.ParentElement )

    val pointLight = new PointLightNode( name )
    pointLight.setTransform( transformation )
    pointLight.setDiffuseColor(         fget( ontology.types.DiffuseColor ) )
    pointLight.setSpecularColor(        fget( ontology.types.SpecularColor ) )
    pointLight.setLinearAttenuation(    fget( oTypes.LinearAttenuation ) )
    pointLight.setConstantAttenuation(  fget( oTypes.ConstantAttenuation ) )
    pointLight.setQuadraticAttenuation( fget( oTypes.QuadraticAttenuation ) )

    addInsertion(entity){ e : Entity =>
      val hullNode = insertNode (e, pointLight, transformation, parentElement )
      connect(e,
        asTriple(ontology.types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform),
        asTriple(ontology.types.DiffuseColor , pointLight.setDiffuseColor( _ : Color), pointLight.getDiffuseColor),
        asTriple(ontology.types.SpecularColor, pointLight.setSpecularColor( _ : Color), pointLight.getSpecularColor),
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
        setParent( skyBox, sceneRoot )
        skyBoxes = skyBoxes + (camera -> skyBox)
      }
    }

    for( w <- windows ) {
      for( (_,mc ) <- mirrorCameras(w) ) {
        for( (_,camera) <- mc ) {
          val skyBox = creater.create( camera.getName )
          setParent( skyBox, sceneRoot )
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
    val parentElement   = None//aget( oTypes.ParentElement )
    val transformation  = fresh.firstValueFor( ontology.types.Transformation )
    val scale           = aget( ontology.types.Scale )

    ask( ResourceManager.self, AskColladaFile(new File(fileName))  ){
      source : GroupNode =>
        val shape = subElement.collect{ case name => Finder.find( source, classOf[GroupNode], name ) }.getOrElse(source)
        val shaderEffect = aspect.getCreateParams.getFirstValueFor( ontology.types.ShaderEffect )

        entityToDirectParentOfShape = entityToDirectParentOfShape.updated(entity, shape)

        shaderEffect.collect {
          case se =>

            val node = Finder.find( shape, classOf[ShapeNode], null )
            node.setMaterial( se.getShaderMaterial )
            shaderEffectMap = shaderEffectMap.updated(node, se)
        }

        addInsertion(entity){ e : Entity =>
          val hullNode = insertNode(e, shape, transformation, parentElement, scale)
          connect(e, asTriple(ontology.types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform))
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
        setParent( clipPlane, hullNode )
        setParent( shape, hullNode )
        hullNode.setTransform( transformation )
        setParent( hullNode, sceneRoot )
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
                setParent( skyBox, sceneRoot )
                skyBoxes = skyBoxes + (mirrorCamera -> skyBox)

              case None =>
            }
          }
        }
        effectplanes = effectplanes ::: name :: Nil

        refreshPipelines()

        connect(entity, asTriple(ontology.types.Transformation, hullNode.setTransform( _ : Transform ), hullNode.getTransform))
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
    val transformation = fresh.firstValueFor(ontology.types.Transformation)
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


  private def createMesh(entity : Entity, aspect : EntityAspect, given : SValSet){
    val createParams = aspect.getCreateParams
    //
    val scale                = aspect.getCreateParams.firstValueFor( simx.components.renderer.jvr.ontology.types.Scale )
    val transformation       = createParams.firstValueFor( simx.components.renderer.jvr.ontology.types.Transformation )
    val material             = makeMeshMaterial( given.firstValueFor( simx.core.ontology.types.Texture ))
    val geometry             = asMesh( given.firstValueFor( simx.core.ontology.types.Mesh ) )
    //
    val shape     = new ShapeNode("mesh_" + entity.id, geometry, material)
    val scaleNode = new GroupNode( shape.getName + "_scale" )
    val hullNode  = new GroupNode( shape.getName + "_hull" )
    //
    shape.setTransform( new Transform )
    hullNode.setTransform( transformation )
    scaleNode.setTransform( scale )

    setParent( scaleNode, hullNode )
    setParent( shape, scaleNode )

    entityToNodeMap     = entityToNodeMap     + (entity -> List(hullNode))
  }

  private def insertMesh( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect)  {
    val createParams = aspect.getCreateParams
    val parentElement  = createParams.getFirstValueFor( simx.core.ontology.types.ParentElement )
    entityToNodeMap.get(entity).collect { case hullNodeList =>
      val hullNode = hullNodeList.head
      parentElement match {
        case Some( e ) if entityToNodeMap contains entity => entityToNodeMap( entity ).head.addChildNode( hullNode ) //TODO: maybe BAD
        case None => sceneRoot.addChildNode( hullNode )
      }

      //update mesh
      //TODO: check if the node has been removed
      getShapeChild(hullNode).collect {
        case shapeNode =>
          entity.getSVars(simx.core.ontology.types.Mesh).collect   { case svar => svar._2.observe{ mesh => shapeNode.setGeometry(asMesh(mesh)) } }
          entity.getSVars(simx.core.ontology.types.Mesh).collect   { case svar => svar._2.get{ mesh => shapeNode.setGeometry(asMesh(mesh)) } }
          entity.getSVars(simx.core.ontology.types.Texture).collect{ case svar => svar._2.observe{ tex => shapeNode.setMaterial(makeMeshMaterial(tex)) } }
          entity.getSVars(simx.core.ontology.types.Texture).collect{ case svar => svar._2.get{ tex => shapeNode.setMaterial(makeMeshMaterial(tex)) } }
      }

      addSVarUpdateFunctions(
        entity.getSVars( simx.components.renderer.jvr.ontology.types.Transformation ).head._2,      //TODO: maybe BAD
        (t: Transform) => hullNode.setTransform(t),
        hullNode.getTransform
      )
      sender ! ElementInjected( entity, aspect )
    }



  }

  private def asMesh(in : Any) : Geometry = in match{
    case geom : Geometry => geom
    case something => throw new Exception("Error " + something.asInstanceOf[AnyRef].getClass.getCanonicalName +
      " is no instance of de.bht.jvr.core.Geometry")
  }

  private def makeMeshMaterial(tex : Any) : ShaderMaterial = {
    val sm = new ShaderMaterial
    val avs = new java.io.File("pipeline_shader/myShader.vs")
    val afs = new java.io.File("pipeline_shader/myShader.fs")
    val amb = new ShaderProgram(avs, afs)


    val dvs = new java.io.File("pipeline_shader/phong_lighting.vs")
    val dfs = new java.io.File("pipeline_shader/phong_lighting.fs")
    val dmb = new ShaderProgram(dvs, dfs)

    sm.setShaderProgram("AMBIENT", amb)
    sm.setUniform("AMBIENT",  "jvr_Material_Ambient",   new UniformColor(new Color(0.3f, 0.3f, 0.3f, 1.0f)))

    sm.setShaderProgram("LIGHTING", dmb)
    sm.setUniform("LIGHTING", "jvr_Material_Diffuse",   new UniformColor(new Color(0.5f, 0.5f, 0.5f, 1.0f)))
    sm.setUniform("LIGHTING", "jvr_Material_Specular",  new UniformColor(new Color(0.5f, 0.5f, 0.5f, 1.0f)))
    sm.setUniform("LIGHTING", "jvr_Material_Shininess", new UniformFloat(10))

    tex match {
      case tex : TextureData =>
        val t = new Texture2D(tex.size.x, tex.size.y, tex.data)
        sm.setTexture("AMBIENT",  "jvr_Texture0", t)
        sm.setTexture("LIGHTING", "jvr_Texture0", t)
        sm.setUniform("LIGHTING", "jvr_UseTexture0", new UniformBool(true))
        sm.setUniform("AMBIENT",  "jvr_UseTexture0", new UniformBool(true))
      case _ =>
        sm.setUniform("AMBIENT",  "jvr_UseTexture0", new UniformBool(false))
        sm.setUniform("LIGHTING", "jvr_UseTexture0", new UniformBool(false))
    }

    sm
  }

  private def getShapeChild( parent : GroupNode) : Option[ShapeNode] = {
    val iterator = parent.getChildNodes.iterator()
    while (iterator.hasNext){
      val found = parent.getChildNodes.iterator().next() match {
        case shapeNode : ShapeNode => Some(shapeNode)
        case groupNode : GroupNode => getShapeChild(groupNode)
        case _ => None
      }
      if (found.isDefined)
        return found
    }
    None
  }

  private def createExistingNode( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect ) {
    val subElement = aspect.getCreateParams.firstValueFor( oTypes.SubElement )
    val n : SceneNode = Finder.find( sceneRoot, classOf[SceneNode], subElement )
    val worldTransform = n.getWorldTransform( sceneRoot )
    val scale = aspect.getCreateParams.firstValueFor(ontology.types.Scale)
    val shaderEffects = aspect.getCreateParams.getAllValuesFor( ontology.types.ShaderEffect )

    if( shaderEffects.nonEmpty ) {
      val shaderEffect = aspect.getCreateParams.firstValueFor( ontology.types.ShaderEffect )
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
      singleConnect[Transform](e, (ontology.types.Transformation, (in : Transform ) => hullNode.setTransform( in ), hullNode.getTransform), useGet = true)
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
      entity.getSVars(um.ontologyMember.get).head._2.set( um.converter._2(um.value) )
    }

    for( uniformManager <- ppe.uniformList ) {
      if( uniformManager.ontologyMember.isDefined ){
        uniformManager.value match {
          case v : TextureData =>
            entity.getSVars(uniformManager.ontologyMember.get).head._2.asInstanceOf[StateParticle[TextureData]].set(new TextureData(ConstVec2i(v.size.x, v.size.y), v.data))
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[TextureData]].observe{
              tex => ppe.getShaderMaterial.setTexture(ppe.nameOfEffect.get, uniformManager.name, new Texture2D(tex.size.x, tex.size.y, tex.data))
            }
          case v : Float =>
            setInitialValue(uniformManager)
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Float]].observe( ( v : Float ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformFloat( v ) ) } )
          case v : Int =>
            setInitialValue(uniformManager)
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Int]].observe( ( v : Int ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformFloat( v ) ) } )
          case v : Boolean =>
            setInitialValue(uniformManager)
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Boolean]].observe( ( v : Boolean ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformBool( v ) ) } )
          case v : Vector2 =>
            setInitialValue(uniformManager)
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[Vector2]].observe( ( v : Vector2 ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector2( v ) ) } )
          case v : ConstVec4f =>
            setInitialValue(uniformManager)
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[ConstVec4f]].observe( ( v : ConstVec4f ) => { ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector4( new Vector4(v.x, v.y, v.z, v.w) ) ) } )
          case v : List[_] =>
            setInitialValue(uniformManager)
            entity.getSVars( uniformManager.ontologyMember.get ).head._2.asInstanceOf[SVar[List[Any]]].observe{ list =>
              ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name + "_size", new UniformInt( list.size ) )
              if( list.isEmpty ) {
                if(simx.core.ontology.types.Vector4List >@ uniformManager.ontologyMember.get)
                  ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector4( new Vector4( 0.0f, 0.0f, 0f, 0f ) ) )
                else
                  ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector2( new Vector2( 0.0f, 0.0f ) ) )
              } else {
                list.head match {
                  case h : Vec2f =>
                    val jvrList = list.asInstanceOf[List[Vec2f]].map(s3dVec => {new Vector2(s3dVec.x, s3dVec.y)})
                    ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector2( jvrList.toArray : _* ) )
                  case h : ConstVec4f =>
                    val jvrList = list.asInstanceOf[List[ConstVec4f]].map(s3dVec => {new Vector4(s3dVec.x, s3dVec.y, s3dVec.z, s3dVec.w)})
                    ppe.getShaderMaterial.setUniform( ppe.nameOfEffect.get, uniformManager.name, new UniformVector4( jvrList.toArray : _* ) )
                }
              }
            }
        }
      }
    }
  }

  private def createPostProcessingEffect( sender: SVarActor.Ref, entity : Entity, aspect : EntityAspect ) {
    val ppe = aspect.getCreateParams.getFirstValueFor( simx.components.renderer.jvr.ontology.types.PostProcessingEffect ).get
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