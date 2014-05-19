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


import simx.core.components.renderer.setup._
import simx.core.entity.description._
import simx.core.components.renderer.{GraphicsComponentAspect, GraphicsComponent}
import simx.core.component.Frequency
import de.bht.jvr.math.Matrix4
import simx.core.svaractor.SVarActor
import simx.core.ontology.Symbols
import simx.core.components.io.IODeviceProvider
import simx.core.entity.typeconversion.{Converter, ConvertibleTrait}
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.Entity
import simx.core.ontology.entities.User
import ontology.types._
import simx.core.ontology.types.{DisplaySetupDescription, Color}
import simx.core.components.renderer.createparameter.VRUser
import simx.core.components.renderer.messages.{EffectsConfiguration, ConfigureRenderer}
import simx.core.ontology.EntityDescription
import simx.core.component.Triggered
import simx.core.helper.TextureData
import simplex3d.math.floatx.ConstMat4f
import simx.core.worldinterface.naming.NameIt

/**
 * The binding for the jVR renderer.
 *
 * This components is a binding between the SimulatorX system and the jVR renderer. It abstracts several features
 * of jVR and makes them usable in Simulator X.
 *
 * The following code contains a small example that starts a JVRConnector that opens a window. The code needs to be
 * executed within a [[simx.core.svaractor.SVarActor]] that also mixes in
 * [[simx.core.component.ExecutionStrategyHandling]].
 *
 * {{{
    val displaySetupDesc = new DisplaySetupDesc
    val displayDesc = new DisplayDesc( Some( 640, 480), (27.24, 16.3), ConstMat4( Mat3x4.translate( Vec3( 0.0f, 0.0f, -23.45f ) ) ), new CamDesc( 0, Eye.RightEye )  )
    val displayDevice = new DisplayDevice( None, displayDesc :: Nil, LinkType.SingleDisplay )
    displaySetupDesc.addDevice( displayDevice, 0 )

    createActor[JVRConnector]()( (renderer) => {
      renderer ! ConfigureRenderer( displayDesc, EffectsConfiguration( "high","none" )
      val executionStrategy = ExecutionStrategy where renderer runs Soft( 60 )
      this.start( executionStrategy )
    )
 * }}}
 *
 * @author Stephan Rehfeld
 *
 * @param name The name of the component. Typically 'renderer.
 *
 */
class JVRConnector( name: Symbol = 'renderer ) extends GraphicsComponent(name) with IODeviceProvider with EntityCreationHandling {

  def this() = this( 'renderer )

  require( componentName != null, "The parameter 'componentName' must not be 'null'!" )


  /**
   * The list of render actors.
   */
  private var renderActors = List[SVarActor.Ref]()

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) = {
    configure(aspect.getCreateParams)
    SValSet()
  }

  protected def finalizeConfiguration(e: Entity){}

  /**
   * The list of render actors that are triggered by this component.
   */
  private var renderActorsToTrigger = List[SVarActor.Ref]()

  /**
   * The list of render actors.
   */
  private var user : List[Entity] = List()

  /**
   * The list of actors that are notified if a render window has been closed.
   */
  private var closeObserver : List[SVarActor.Ref] = List()

  /**
   * The entity description for the user entity.
   */
  private val userDesc = new EntityDescription( VRUser(), NameIt("User") )

  /**
   * The amount of render actors that already has been configured. This flag is used while starting and configuring all
   * render actors.
   */
  private var renderActorsConfigured = 0

  /**
   * This map holds open publish request while creating an entity.
   */
  private var openPublishRequests = Map[Entity, SValSet]()

  /**
   * This map holds a counter how many render actors already anserwered.
   */
  private var openPublishRequestsCounter = Map[Entity, Int]()

  /**
   * This map holds open transformation request while creating an entity.
   */
  private var openTransformationRequests = Map[Entity, (SValSet, SValSet)]()


  /*
   *
   * Configuration of the component
   *
   */
  override protected def configure(params: SValSet) {
    info( "Got configuration" )
    if( JVRConnector.amountOfUser( params.firstValueFor(DisplaySetupDescription) ).intValue() == 1 ) {
      val grouped =  JVRConnector.sortToGroups( JVRConnector.createRenderActorConfigs( params.firstValueFor(DisplaySetupDescription) ) )
      process( grouped, params.firstValueFor(simx.core.ontology.types.EffectsConfiguration) )
      info( "1 User, processing, Creating user entity " )
    }
  }

  addHandler[SwitchEyes]{
    msg =>
      renderActors.foreach(_ ! msg)
  }

  addHandler[ConfigureRenderer]{msg =>
    throw new Exception("[JVRConnector] received deprecated ConfigureRenderer message. Use the arguments of JVRComponentAspect to configure JVRComponent insted.")
  }

  addHandler[JVRRenderActorConfigComplete] { msg =>
    renderActorsConfigured = renderActorsConfigured + 1
  }

  addHandler[MeshCreated]{
    msg => provideInitialValues(msg.entity, msg.initialValues)
  }

  /**
   * This message process a preprocessed form of the display description and created instances of
   * [[simx.components.renderer.jvr.JVRRenderActor]].
   *
   * @param grouped A preprocessed form the display description.
   * @param effectsConfiguration The effectsConfiguration message.
   */
  private def process( grouped : Map[Int,(List[RenderActorConfig],Frequency)], effectsConfiguration: EffectsConfiguration ) {
    require( grouped != null, "The parameter 'grouped' must not be 'null'" )
    require( effectsConfiguration != null, "The parameter 'effectsConfiguration' must not be 'null'" )

    if( grouped.isEmpty ) {
      userDesc.realize{ e =>
        publishDevice(simx.core.ontology.types.User(new User(e, actorContext)))
        renderActors.foreach( _ ! JVRPublishUserEntity(e) )
      }
      heldPublishElementTasks.foreach(task => {publishElement(task.e, task.aspect, task.ready, task.given)})
      heldPublishElementTasks = Nil
    }
    else {
      val (id,(configs,frequency)) = grouped.head
      if( configs.head.node.isDefined ) {
        val node = configs.head.node.get
        info( "Creating Render Actor for display group {} on node {}", id, node )

        createActor( new JVRRenderActor, Some( node ))( (renderActor) => {
          info( "Sending Config msg to Render Actor" )
          renderActor ! RenderActorConfigs( id, effectsConfiguration.shadowQuality, effectsConfiguration.mirrorQuality, configs, frequency )

          trace( "Adding render actor to list of render actors" )
          renderActors = renderActors ::: renderActor :: Nil
          if( frequency == Triggered() ) renderActorsToTrigger = renderActorsToTrigger ::: renderActor :: Nil
          process( grouped - id, effectsConfiguration )
        })()

      } else {
        info( "Creating Render Actor for display group {}", id )
        val actor = createActor(new JVRRenderActor()){ renderActor => }()
        info( "Sending Config msg to Render Actor" )
        actor ! RenderActorConfigs( id, effectsConfiguration.shadowQuality, effectsConfiguration.mirrorQuality, configs, frequency )
        trace( "Adding render actor to list of render actors" )
        renderActors = renderActors ::: actor :: Nil
        if( frequency == Triggered() ) renderActorsToTrigger = renderActorsToTrigger ::: actor :: Nil
        process( grouped - id, effectsConfiguration )
      }
    }
  }

  /**
   * returns a set of additional convertibletraits specifying the addidional svars provided by this component
   * @return the set
   */
  override protected def getAdditionalProvidings(aspect: EntityAspect) = {
    if (aspect.semanticsEqual(Symbols.shapeFromFile))
      Set(simx.core.ontology.types.Texture)
    else
      Set()
  }

  /*
   *
   * Entity creation
   *
   */
  override protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity, given: SValSet) {
    info( "Intitial value requested for {}", aspect.getCreateParams.semantics.value )
    val (ready, remaining) = aspect.getCreateParams.combineWithValues( toProvide )
    //ready = given.xMergeWith(ready)

    if (remaining.isEmpty) {
      if (aspect.semanticsEqual(Symbols.meshComponent))
        renderActors.foreach( _ ! CreateMesh(e, aspect, given, ready, self))
      else
        publishElement(e, aspect, ready, given)
    }


    else aspect.getCreateParams.semantics match {
      case Symbols.existingNode => renderActors.headOption match{
        case None =>
          throw new Exception("no render actors found, cannot create entity for aspect " + aspect)
        case Some(ra) =>
          openTransformationRequests = openTransformationRequests.updated(e, (ready, given))
          ra ! RequestTransformation( aspect.getCreateParams.firstValueFor( simx.core.ontology.types.SubElement ), e, aspect )
      }

      case Symbols.postProcessingEffect =>
        val ppe = aspect.getCreateParams.getFirstSValFor( simx.components.renderer.jvr.ontology.types.PostProcessingEffect ).get.value
        for( sVarDescription <- remaining )
          ready.addIfNew( combine( sVarDescription, ppe ) )
        publishElement(e, aspect, ready, given)
      case Symbols.shapeFromFile =>
        ready.addIfNew(simx.core.ontology.types.Texture(JVRConnector.voidTextureData))
        publishElement(e, aspect, ready, given)
      case Symbols.`meshComponent` =>
        renderActors.foreach( _ ! CreateMesh(e, aspect, given, ready, self))
      case somethingUnexpected =>
        throw new Exception( remaining + " is missing for " + somethingUnexpected)
    }
  }

  addHandler[TellTransformation]{ msg : TellTransformation =>
    val (ready, given) = openTransformationRequests( msg.entity )
    openTransformationRequests = openTransformationRequests - msg.entity
    ready.addIfNew( simx.core.ontology.types.Transformation( JVRConnector.transformConverter.convert( msg.transformation ) ) )
    publishElement(msg.entity, msg.aspect, ready, given)
  }

  // Helper function
  /**
   * An internal helper function to process a [[simx.components.renderer.jvr.PostProcessingEffect]]
   * @param c The convertible trait that descripes to convertion between local and systemwide data. It's used to create the SVar that's manipulating a parameter of the effect.
   * @param ppe The description of the post processing effect.
   * @tparam T The type of the parameter.
   * @return An SVal to create the SVar.
   */
  private def combine[T]( c : ConvertibleTrait[T], ppe : PostProcessingEffect ) : SVal[T] =
    c( ppe.getValueForSVarDescription( c ) )

  private case class PublishElementTask(e : Entity, aspect : EntityAspect, ready : SValSet, given : SValSet)
  private var heldPublishElementTasks: List[PublishElementTask] = Nil

  /**
   * This method publishes an entity to the render actors.
   *
   * @param e The entity to publish.
   * @param aspect The configuration aspect of the entity.
   * @param ready An SVal list that contains several parameters.
   */
  private def publishElement( e : Entity, aspect : EntityAspect, ready : SValSet, given : SValSet ) {
    require( e != null, "The parameter 'e' must not be 'null'" )
    require( aspect != null, "The parameter 'aspect' must not be 'null'" )
    require( ready != null, "The parameter 'ready' must not be 'null'" )

    if (aspect.semanticsEqual(Symbols.user) || aspect.semanticsEqual(Symbols.component))
      provideInitialValues(e, ready)
    else if(renderActors.isEmpty) {
      println("WARNING: JVR initialization was not complete before this entity creation request")
      heldPublishElementTasks = PublishElementTask(e, aspect, ready, given) :: heldPublishElementTasks
    }
    else{
      info("publishing " + aspect.getCreateParams.semantics.value)
      openPublishRequests = openPublishRequests.updated(e, ready)
      renderActors.foreach{ _ ! PublishSceneElement( e, aspect, new SValSet(given).xMergeWith(ready) ) }
      openPublishRequestsCounter = openPublishRequestsCounter + (e -> renderActors.size )
    }
  }

  addHandler[ElementInjected]{ msg =>
    //TODO: Fix bug related with the usage of EntityAspect as key for openPublishRequestsCounter and openPublishRequests
    openPublishRequestsCounter = openPublishRequestsCounter.updated( msg.entity, openPublishRequestsCounter( msg.entity) - 1 )

    if( openPublishRequestsCounter( msg.entity ) == 0 ) {
      openPublishRequests.get(msg.entity) collect{
        case pReq => provideInitialValues(msg.entity, pReq)
      }
      openPublishRequestsCounter = openPublishRequestsCounter - msg.entity
    }
  }

  protected def entityConfigComplete(e: Entity , aspect: EntityAspect){
    if (aspect.semanticsEqual(Symbols.component)){
      //
    } else if( aspect.semanticsEqual(Symbols.user) ) {
      trace( "Adding to list of users " )
      user = user ::: e :: Nil
    } else {
      //if (openPublishRequests.get(e).isEmpty)
      //  publishElement(e, aspect, new SValSet(), new SValSet())
      openPublishRequests = openPublishRequests - e

      info( "Got new completed of Type {} entity and distribute it to {} actors",
        aspect.getCreateParams.getFirstValueFor(simx.core.ontology.types.Name),
        renderActors.size )
      renderActors.foreach( _ ! InsertEntity(e))
    }
  }

  /*
   *
   * Attaching and detaching a scene element.
   *
   */
  addHandler[DetachObject]{
    msg => renderActors.foreach( _ ! msg )
  }

  addHandler[AttachObject]{
    msg => renderActors.foreach( _ ! msg )
  }



  addHandler[RegroupEntity] {
    case m : RegroupEntity => renderActors.foreach( _ ! m )

  }

  addHandler[JVRRenderWindowClosed] { msg =>
    for( o <- closeObserver ) o ! msg
  }

  addHandler[SetAmbientColor]{
    msg => renderActors.foreach( _ ! msg )
  }

  addHandler[NotifyOnClose] { case msg =>
    closeObserver = closeObserver ::: msg.sender :: Nil
  }

  addHandler[PinToCam]{
    msg => renderActors.foreach( _ ! msg )
  }

  info( "Raised and waiting for configuration" )

  var frameBegins = List[Long]()

  var firstTime = true

  override protected def performSimulationStep() {
    if( firstTime ) {
      firstTime = false
      for( r <- renderActors ) {
        if( !renderActorsToTrigger.contains( r ) ) {
          r ! RenderNextFrame()
        }
      }
    }

    if(JVRDebugSettings.printFPS) {
      if( frameBegins.size > 1 ) {
        if( (frameBegins.reverse.head - frameBegins.head) > 10000 ) {
          val time = (frameBegins.reverse.head - frameBegins.head).asInstanceOf[Double] / 1000.0
          println( frameBegins.size.asInstanceOf[Double] / time )
          frameBegins = List()
        }
      }
      frameBegins = frameBegins ::: System.currentTimeMillis() :: Nil
    }

    for( observer <- this.renderStepObserver ) observer ! FrameStarted()
    if (!renderActorsToTrigger.isEmpty)
      renderActorsToTrigger.foreach( _ ! RenderNextFrame( ) )
    else
      this.addJobIn( 16 ){this.simulationCompleted()}
  }

  /**
   * This counter is used to count how many render actors already finished the frame.
   */
  var finishedRenderActors = 0

  addHandler[FinishedFrame] {
    msg =>
      finishedRenderActors = finishedRenderActors + 1
      if( finishedRenderActors == renderActors.size ) {
        finishedRenderActors = 0
        for( observer <- this.renderStepObserver ) observer ! FrameFinished()
        this.simulationCompleted()
      }
  }

  override def removeFromLocalRep( e: Entity ) {
    require( e != null, "The parameter 'e' must not be 'null'!" )
    for( r <- renderActors ) {
      r ! RemoveSceneElement( e )
    }
  }

  /*
   *
   *  ------------------------------------------------------------- Render step observer.
   *
   *
   */

  private var renderStepObserver = Set[SVarActor.Ref]()

  addHandler[SubscribeForRenderSteps] {
    msg =>
      renderStepObserver = renderStepObserver + msg.sender
  }

  addHandler[UnsubscribeForRenderSteps] {
    msg =>
      renderStepObserver = renderStepObserver - msg.sender
  }


  addHandler[PrintStepCountOf] {
    msg =>
      for( r <- renderActors ) {
        r ! msg
      }
  }
}

/**
 * The companion object of the JVRConnector. It contains some functions to interpret the display setup
 * description and transform it into a configuration for a JVRRenderActor.
 *
 * @author Stephan Rehfeld
 */
object JVRConnector {

  val voidTextureData = TextureData(simplex3d.math.ConstVec2i(0,0), Array[Byte]())

  def isVoidTextureData(d: TextureData): Boolean =
    d.id == voidTextureData.id

  /**
   * This methods interprets a display setup description and returns the amount of users.
   *
   * @param displaySetup The display setup.
   * @return The amount of users in the setup description.
   */
  def amountOfUser( displaySetup: DisplaySetupDesc ) : java.lang.Integer = {
    val eyeIds = collection.mutable.Set[java.lang.Integer]()
    for( (id,group) <- displaySetup.deviceGroups ) {
      for( displayDevice <- group.dpys ) {
        for( displayDesc <- displayDevice.displayDescs ) {
          eyeIds += displayDesc.view.camId
        }
      }
    }
    eyeIds.size
  }

  /**
   * This function returns the amount of render windows in the display setup description.
   *
   * @param displaySetup The display setup description.
   * @return The amount of render windows in the display setup description.
   */
  def amountOfRenderWindows( displaySetup: DisplaySetupDesc ) : java.lang.Integer = {
    var windows = 0
    for( (id,group) <- displaySetup.deviceGroups ) {
      for( displayDevice <- group.dpys ) {
        if( displayDevice.linkType == LinkType.AnaglyphStereo || displayDevice.linkType == LinkType.TopBottomStereo) {
          windows += 1
        } else for( displayDesc <- displayDevice.displayDescs ) {
          windows += 1
        }
      }
    }
    windows
  }

  /**
   * This function returns the amount of anaglyph render windows in the display setup description.
   *
   * @param displaySetup The display setup description.
   * @return The amount of anaglyph render windows in the display setup description.
   */
  def amountOfAnaglyphWindows( displaySetup: DisplaySetupDesc ) : java.lang.Integer = {
    var windows = 0
    for( (id,group) <- displaySetup.deviceGroups ) {
      for( displayDevice <- group.dpys ) {
        if( displayDevice.linkType == LinkType.AnaglyphStereo ) {
          windows += 1
        }
      }
    }
    windows
  }

  /**
   * This function creates RenderActorConfig messages for all single windows.
   *
   * @param displaySetup The display setup description.
   * @return A set of all configurations as tupels of display group and config.
   */
  def createSingleDisplayRenderActorConfigs( displaySetup : DisplaySetupDesc)(implicit self : SVarActor.Ref) : Set[(Int,RenderActorConfig,Frequency)] = {
    var configs = Set[(Int,RenderActorConfig,Frequency)]()
    for( (id,group) <- displaySetup.deviceGroups ) {
      for( displayDevice <- group.dpys ) {
        if( displayDevice.linkType == LinkType.SingleDisplay ) {
          for( displayDesc <- displayDevice.displayDescs ) {
            val v = (id,new RenderActorConfig(
              'xyz,
              displayDevice.hardwareHandle,
              displayDesc.resolution,
              displayDesc.size,
              displayDesc.transformation,
              eyeToEyeToRender( displayDesc.view.eye ),
              None, displayDevice.node, displayDesc.view.eyeSeparation ), displaySetup.getFrequencyOf( id ) )
            configs += v
          }
        }
      }
    }
    configs
  }

  /**
   * This function creates RenderActorConfig messages for all stereo windows.
   *
   * @param displaySetup The display setup description.
   * @return A set of all configurations as tupels of display group and config.
   */
  def createStereoDisplayRenderActorConfigs( displaySetup : DisplaySetupDesc)(implicit self : SVarActor.Ref) : Set[(Int,RenderActorConfig,Frequency)] = {
    var configs = Set[(Int,RenderActorConfig,Frequency)]()
    for( (id,group) <- displaySetup.deviceGroups ) {
      for( displayDevice <- group.dpys ) {
        if( displayDevice.linkType == LinkType.AnaglyphStereo ) {
          val displayDesc = displayDevice.displayDescs( 0 )
          val v = (id, new RenderActorConfig(
            'xyz,
            displayDevice.hardwareHandle,
            displayDesc.resolution,
            displayDesc.size,
            displayDesc.transformation,
            EyeToRender.Both,
            Some(StereoMode.AnaglyphStereo), displayDevice.node, displayDesc.view.eyeSeparation ), displaySetup.getFrequencyOf( id ))
          configs += v

        } else if( displayDevice.linkType == LinkType.FrameSequential ) {
          val displayDesc = displayDevice.displayDescs( 0 )
          val v = (id, new RenderActorConfig(
            'xyz,
            displayDevice.hardwareHandle,
            displayDesc.resolution,
            displayDesc.size,
            displayDesc.transformation,
            EyeToRender.Both,
            Some(StereoMode.FrameSequential), displayDevice.node, displayDesc.view.eyeSeparation ), displaySetup.getFrequencyOf( id ))
          configs += v

        } else if( displayDevice.linkType == LinkType.TopBottomStereo) {
          val displayDesc = displayDevice.displayDescs( 0 )
          val v = (id, new RenderActorConfig(
            'xyz,
            displayDevice.hardwareHandle,
            displayDesc.resolution,
            displayDesc.size,
            displayDesc.transformation,
            EyeToRender.Both,
            Some(StereoMode.TopBottomStereo), displayDevice.node, displayDesc.view.eyeSeparation ), displaySetup.getFrequencyOf( id ))
          configs += v

        }
      }
    }
    configs
  }

  /**
   * This function converts from the display setup description eye to the internal eye enum.
   *
   * @param eye the display setup description eye.
   * @return The value of the internal eye enum.
   */
  def eyeToEyeToRender( eye: Eye.Value ) : EyeToRender.Value =
    if( eye == Eye.LeftEye ) EyeToRender.Left else EyeToRender.Right

  /**
   * This function converts from the internal eye to the display setup description eye enum.
   *
   * @param eyeToRender The internal eye enum value.
   * @return The display setup description eye value.
   */
  def EyeToRenderToEye( eyeToRender: EyeToRender.Value ) : Eye.Value =
    if( eyeToRender == EyeToRender.Left ) Eye.LeftEye else Eye.RightEye

  /**
   * This function creates RenderActorConfig messages for all windows.
   *
   * @param setupDescription The display setup description.
   * @return A set of all configurations as tupels of display group and config.
   */
  def createRenderActorConfigs( setupDescription : DisplaySetupDesc)(implicit self : SVarActor.Ref) : Set[(Int,RenderActorConfig,Frequency)] = {
    createSingleDisplayRenderActorConfigs( setupDescription) union createStereoDisplayRenderActorConfigs( setupDescription)
  }

  /**
   * This function resorts the display description.
   *
   * @param configs In
   * @return Out
   */
  def sortToGroups( configs: Set[(Int,RenderActorConfig,Frequency)] ) : Map[Int,(List[RenderActorConfig],Frequency)] = {
    var transformed : Map[Int,(List[RenderActorConfig],Frequency)] = Map()
    for( (id,config,frequency) <- configs ) {
      if( !transformed.contains( id ) ) {
        transformed = transformed + (id -> (List[RenderActorConfig](),frequency))
      }
      transformed = transformed + (id -> (transformed(id)._1 ::: config :: Nil,frequency) )
    }
    transformed
  }

  /**
   * This converters converts between a simplex3d matrix and a transform object of OntologyJVR.
   */
  val transformConverter = new Converter(Transformation, Scale, ViewPlatform, HeadTransform)(simx.core.ontology.types.Transformation) {

    override def canRevert(to: ConvertibleTrait[_], from: ConvertibleTrait[_]) = true

    override def canConvert(from: ConvertibleTrait[_], to: ConvertibleTrait[_]) = true

    override def revert(from: ConstMat4f) : de.bht.jvr.core.Transform =

      new de.bht.jvr.core.Transform(
        new Matrix4(
          from.m00, from.m10, from.m20, from.m30,
          from.m01, from.m11, from.m21, from.m31,
          from.m02, from.m12, from.m22, from.m32,
          from.m03, from.m13, from.m23, from.m33 )
      )

    override def convert(from: de.bht.jvr.core.Transform): ConstMat4f = {
      val matrix = from.getMatrix
      ConstMat4f(
        matrix.get( 0, 0 ), matrix.get( 1, 0 ), matrix.get( 2, 0 ), matrix.get( 3, 0 ),
        matrix.get( 0, 1 ), matrix.get( 1, 1 ), matrix.get( 2, 1 ), matrix.get( 3, 1 ),
        matrix.get( 0, 2 ), matrix.get( 1, 2 ), matrix.get( 2, 2 ), matrix.get( 3, 2 ),
        matrix.get( 0, 3 ), matrix.get( 1, 3 ), matrix.get( 2, 3 ), matrix.get( 3, 3 )
      )
    }
  }

  //  val textureConverter = new Converter(types.Texture)(simx.core.ontology.types.Texture) {
  //      protected def b2i(b : Byte) : Int = if (b < 0) 256 + b else b.toInt
  //      protected def intToArray(v : Int) = (for ( i <- List(1, 256, 65536) ) yield ( (v / i) % 256).toByte).toArray
  //      protected def arrayToInt(a : Array[Byte]) = a.foldLeft((0,1))((t, b) => (t._1 + b2i(b) * t._2, t._2 * 256) )._1
  //
  //      override def canRevert(to: ConvertibleTrait[_], from: ConvertibleTrait[_]) = true
  //      //override def canConvert(from: TypeInfo[_], to: TypeInfo[_]) = true
  //
  //      override def revert(from: Array[Byte]) : de.bht.jvr.core.Texture2D =
  //        new Texture2D(arrayToInt(from.slice(0,3)), arrayToInt(from.slice(3,6)), from.slice(6, from.length))
  //
  //      override def convert(from: de.bht.jvr.core.Texture2D): Array[Byte] =
  //        intToArray(from.getWidth) ++ intToArray(from.getHeight) ++ from.getImageData
  //    }

  //  /**
  //   * This converters converts between a simplex3d matrix and a transform object of OntologyJVR.
  //   */
  //  val transformConverter2 = new Converter[de.bht.jvr.core.Transform, simplex3d.math.floatm.renamed.Mat4x4] {
  //
  //    override def canRevert(to: ConvertibleTrait[_], from: ConvertibleTrait[_]) = true
  //
  //    override def canConvert(from: ConvertibleTrait[_], to: ConvertibleTrait[_]) = true
  //
  //    override def revert(from: simplex3d.math.floatm.renamed.Mat4x4) : de.bht.jvr.core.Transform =
  //
  //      new de.bht.jvr.core.Transform(
  //        new Matrix4(
  //          from.m00, from.m01, from.m02, from.m03,
  //          from.m10, from.m11, from.m12, from.m13,
  //          from.m20, from.m21, from.m22, from.m23,
  //          from.m30, from.m31, from.m32, from.m33 )
  //      )
  //
  //    override def convert(from: de.bht.jvr.core.Transform): simplex3d.math.floatm.renamed.Mat4x4 = {
  //      val matrix = from.getMatrix
  //      simplex3d.math.floatm.renamed.Mat4x4(
  //        matrix.get( 0, 0 ), matrix.get( 1, 0 ), matrix.get( 2, 0 ), matrix.get( 3, 0 ),
  //        matrix.get( 0, 1 ), matrix.get( 1, 1 ), matrix.get( 2, 1 ), matrix.get( 3, 1 ),
  //        matrix.get( 0, 2 ), matrix.get( 1, 2 ), matrix.get( 2, 2 ), matrix.get( 3, 2 ),
  //        matrix.get( 0, 3 ), matrix.get( 1, 3 ), matrix.get( 2, 3 ), matrix.get( 3, 3 )
  //      )
  //    }
  //  }

  /**
   * This converter converts between an AWT color and the color type of jVR.
   */
  val colorConverter = new Converter(DiffuseColor, SpecularColor)(Color){
    override def convert(i: de.bht.jvr.util.Color) =
      new java.awt.Color((i.r*255).toInt, (i.g*255).toInt, (i.b*255).toInt)

    override def revert(i: java.awt.Color) =
      new de.bht.jvr.util.Color(i.getRed.toFloat/255f, i.getGreen.toFloat/255f, i.getBlue.toFloat/255f)
  }

}



/**
 * This enum is used by the internal configuration to describe which eye is rendered by a window.
 *
 * @author Stephan Rehfeld
 */
object EyeToRender extends Enumeration {

  /**
   * The left eye.
   */
  val Left = Value( "Left" )

  /**
   * The right eye.
   */
  val Right = Value("Right")

  /**
   * Both eyes. Normally this means anaglyph.
   */
  val Both = Value("Both")
}

object JVRDebugSettings {
  final val printFPS = false
}


case class JVRComponentAspect(
  name : Symbol,
  displaySetup: DisplaySetupDesc = BasicDisplayConfiguration(1280, 800, fullscreen = false),
  effectsConfiguration : EffectsConfiguration = EffectsConfiguration("low", "none")
)
  extends GraphicsComponentAspect[JVRConnector](name){
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()
  def getCreateParams: NamedSValSet = NamedSValSet(aspectType,
      DisplaySetupDescription(displaySetup),
      simx.core.ontology.types.EffectsConfiguration(effectsConfiguration)
  )
}


