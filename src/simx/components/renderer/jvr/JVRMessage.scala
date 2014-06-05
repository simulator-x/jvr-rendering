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

import de.bht.jvr.util.Color
import simx.core.entity.Entity
import simx.core.components.renderer.messages.RendererMessage
import simx.core.entity.description.{SValSet, EntityAspect}
import de.bht.jvr.core.Transform
import simx.core.component.{Passive, Frequency}
import simx.core.svaractor.SVarActor
import simplex3d.math.floatx.ConstMat4f

/**
 * A common base class for all jVR specific messages.
 *
 * @author Stephan Rehfeld
 */
class JVRMessage(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This messages is used by the [[simx.components.renderer.jvr.JVRConnector]] to publish a entity with the create param
 * to the render actors.
 *
 * @author Stephan Rehfeld
 *
 * @param e The entity object that represents the object in the middle ware.
 * @param aspect The aspect that tells which kind of object should be constructed.
 * @param providings Several value.
 */
private[jvr] case class PublishSceneElement( e : Entity, aspect : EntityAspect, providings : SValSet )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
  //require( aspect != null, "The parameter 'aspect' must not be 'null'!" )
  require( providings != null, "The parameter 'providings' must not be 'null'!" )
}

/**
 * This messages is used by the [[simx.components.renderer.jvr.JVRConnector]] to tell the
 * [[simx.components.renderer.jvr.JVRRenderActor]] to remove an entity from the scene graph.
 *
 * @author Stephan Rehfeld
 *
 * @param e The entity to remove
 */
private[jvr] case class RemoveSceneElement( e : Entity )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
}

/**
 * Tells JVR to set the ambient color for all windows.
 *
 * @author Stephan Rehfeld
 *
 * @param color The new ambient color.
 */
case class SetAmbientColor( color: Color )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( color != null, "The parameter 'color' must not be 'null'!" )
}

/**
 *  Tells JVR to add the camera right below the given node in the SceneGraph
 *
 * @author Martin Fischbach
 */
case class PinToCam( e: Entity, offset: ConstMat4f )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
}


/**
 * Sending this message will regroup an entity within the internal scene graph.
 *
 * @author Stephan Rehfeld
 *
 * @param e The entity that should be regrouped.
 * @param target An optional target. Passing none will group the entity under the root node. Default value is None.
 * @param convertTransform It true the world position of the entity will stay the same. The transformation matrix will be converted. Default value is true.
 *
 */
case class RegroupEntity( e: Entity, target: Option[Entity] = None, convertTransform: Boolean = true )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
  require( target != null, "The parameter 'target' must not be 'null'!" )
}

/**
 * This message will be sent when the regrouping operation has been applied.
 *
 * @author Stephan Rehfeld
 *
 * @param e The entity that should be regrouped.
 * @param target An optional target. See [[simx.components.renderer.jvr.RegroupEntity]].
 */
case class RegroupApplied(e : Entity, target : Option[Entity])(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
  require( target != null, "The parameter 'target' must not be 'null'!" )
}

/**
 * This messages signals the [[simx.components.renderer.jvr.JVRConnector]] to notify the sender if one render window has
 * been closed.
 *
 * @author Stephan Rehfeld
 */
case class NotifyOnClose()(implicit self : SVarActor.Ref) extends JVRMessage

/**
 * This message can be sent to the [[simx.components.renderer.jvr.JVRConnector]] to detach an object from the scene
 * graph. The entity still exists. It can attached again by sending [[simx.components.renderer.jvr.AttachObject]]
 * message.
 *
 * @author Stephan Rehfeld
 *
 * @param e The entity that should be detached from the scene graph.
 */
case class DetachObject( e : Entity )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
}

/**
 * This message can be sent to the [[simx.components.renderer.jvr.JVRConnector]] to attach a previously detached object
 * to the scene graph again.
 *
 * @author Stephan Rehfeld
 *
 * @param e The entity that should be attached to the scene graph.
 */
case class AttachObject( e : Entity )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( e != null, "The parameter 'e' must not be 'null'!" )
}

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * It signals the JVRConnector that the JVRRenderActor is ready.
 *
 * @author Stephan Rehfeld
 */
private[jvr] case class JVRRenderActorConfigComplete()(implicit self : SVarActor.Ref) extends JVRMessage

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * It signals the JVRRenderActor to render the next frame. The sender of this message will receive a
 * [[simx.components.renderer.jvr.FinishedFrame]] message.
 *
 * @author Stephan Rehfeld
 */
protected[jvr] case class RenderNextFrame()(implicit self : SVarActor.Ref) extends JVRMessage

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * It signals the JVRConnector that the render actor has finished to render the frame.
 *
 * @author Stephan Rehfeld
 */
private[jvr] case class FinishedFrame()(implicit self : SVarActor.Ref) extends JVRMessage

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * When an entity is created using the ExistingNode the transformation
 * of the object is only known by the renders actors. The connector component needs to request the transformation from
 * them.
 *
 * The sender of this message will receive a [[simx.components.renderer.jvr.TellTransformation]] message.
 *
 * @author Stephan Rehfeld
 *
 * @param element The name of the node in the scene graph.
 * @param entity The entity that represents the parent node of the scene graph.
 * @param aspect The entity creation aspect.
 */
private[jvr] case class RequestTransformation( element : String, entity : Entity, aspect : EntityAspect )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( element != null, "The parameter 'element' must not be 'null'!" )
  require( entity != null, "The parameter 'entity' must not be 'null'!" )
  require( aspect != null, "The parameter 'aspect' must not be 'null'!" )
}

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * This message is sent as a reply to a [[simx.components.renderer.jvr.RequestTransformation]] message and tells the
 * transformation of the node in world coordinates.
 *
 * @author Stephan Rehfeld
 *
 * @param entity The entity that represents the parent node of the scene graph.
 * @param transformation The transformation of the requested node in world coordinates.
 * @param aspect The entity creation aspect.
 */
private[jvr] case class TellTransformation( entity : Entity, transformation : Transform, aspect : EntityAspect )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( transformation != null, "The parameter 'transformation' must not be 'null'!" )
  require( entity != null, "The parameter 'entity' must not be 'null'!" )
  require( aspect != null, "The parameter 'aspect' must not be 'null'!" )
}

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * This message is sent by the [[simx.components.renderer.jvr.JVRRenderActor]] to the
 * [[simx.components.renderer.jvr.JVRConnector]] after the internal representation of an entity was created and is ready
 * to be integrated into the scene graph.
 *
 * @author Stephan Rehfeld
 *
 * @param entity The entity object.
 * @param aspect The creation aspect of the entity.
 */
private[jvr] case class ElementInjected( entity : Entity, aspect : EntityAspect )(implicit self : SVarActor.Ref) extends JVRMessage {
  require( entity != null, "The parameter 'entity' must not be 'null'!" )
  require( aspect != null, "The parameter 'aspect' must not be 'null'!" )
}

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * This message is sent by the [[simx.components.renderer.jvr.JVRRenderActor]] to the
 * [[simx.components.renderer.jvr.JVRConnector]] if the entity creation is completed. The prepared scene graph element
 * is inserted into the scene.
 *
 * @author Stephan Rehfeld
 *
 * @param toInsert The entity which internal representation should be inserted into the scene graph.
 */
private[jvr] case class InsertEntity( toInsert : Entity )(implicit self : SVarActor.Ref) extends JVRMessage  {
  require( toInsert != null, "The parameter 'toInsert' must not be 'null'!" )
}

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * This message represents a configuration for a JVRRenderActor
 *
 * @author Stephan Rehfeld
 *
 * @param id The id of the render actor.
 * @param hardwareHandle An optional hardware handle, a tupel of display, screen, and channel. If the value is none the OS descrides where the window will be opened.
 * @param resolution A optional resolution of the window. If not set the window will be opened in full screen.
 * @param size The size of the window.
 * @param transformation The transformation of the view panel relative to the view platform.
 * @param eyeToRender The eye that should be renderer on this window
 * @param stereoMode The stereo mode of the window.
 * @param node The node where to run the render actor.
 * @param eyeSeparation The eye separation for stereo mode.
 */
private[jvr] case class RenderActorConfig( id: Symbol, hardwareHandle: Option[(Int,Int,Int)], resolution: Option[(Int, Int)],
                                           size: (Double, Double), transformation: ConstMat4f, eyeToRender : EyeToRender.Value,
                                           stereoMode : Option[StereoMode.Value], node : Option[Symbol], eyeSeparation : Option[Float] )
                                         (implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * A collection of several render actor configurations.
 *
 * @author Stephan Rehfeld
 *
 * @param id The id of the render actor.
 * @param shadowQuality The quality for the shadow fbos.
 * @param mirrorQuality The quality for the mirror fbos.
 * @param configs A list of all configurations for the render actor.
 * @param frequency The frequency for this render actor. [[simx.core.component.Triggered]] means triggered by [[simx.components.renderer.jvr.JVRConnector]]. In other cases the render actor triggers it self.
 */
private[jvr] case class RenderActorConfigs( id: Int, shadowQuality : String, mirrorQuality : String,
                                            configs : List[RenderActorConfig], frequency : Frequency )
                                          (implicit @transient self : SVarActor.Ref) extends RendererMessage {
  require( !frequency.isInstanceOf[Passive], "As passive render actor does not make sense!" )
}

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * This message is send by the JVRRenderActor if a window has been closed.
 *
 * @author Stephan Rehfeld
 */
private[jvr] case class JVRRenderWindowClosed()(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This message is used for the internal communication between the [[simx.components.renderer.jvr.JVRConnector]] and
 * the [[simx.components.renderer.jvr.JVRRenderActor]].
 *
 * This message published the user entity to the render actors.
 *
 * @author Stephan Rehfeld
 *
 * @param user The entity that represents the user.
 */
private[jvr] case class JVRPublishUserEntity( user: Entity )(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This message can be sent to an instance of [[simx.components.renderer.jvr.JVRConnector]] to get notified when a frame
 * has started and is completed.
 *
 * @author Stephan Rehfeld
 */
case class SubscribeForRenderSteps()(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This message can be sent to an instance of [[simx.components.renderer.jvr.JVRConnector]] to quit a subscription
 * enabled with a [[simx.components.renderer.jvr.SubscribeForRenderSteps]] message.
 *
 * @author Stephan Rehfeld
 */
case class UnsubscribeForRenderSteps()(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This message is sent to observers of the render steps, if a frame has started.
 *
 * @author Stephan Rehfeld
 */
case class FrameStarted()(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * This message is sent to observers of the render steps, if a frame has finished.
 *
 * @author Stephan Rehfeld
 */
case class FrameFinished()(implicit @transient self : SVarActor.Ref) extends RendererMessage

/**
 * Message for benchmarking purposes.
 *
 * Send this message to the renderer and the the render actors will observe and print the step count of the source
 * component.
 *
 * @param source The source of the step count.
 */
case class PrintStepCountOf( source : SVarActor.Ref )(implicit @transient self : SVarActor.Ref) extends RendererMessage

case class SwitchEyes() (implicit @transient self : SVarActor.Ref) extends RendererMessage