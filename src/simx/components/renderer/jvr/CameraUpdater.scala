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

import de.bht.jvr.core.{Transform, VRCameraNode, SceneNode}

/**
 * The abstract class CameraUpdater is the base class for every class which keeps two cameras in sync.
 * It it used internally for mirror effects.
 *
 * @author Stephan Rehfeld
 */
private[jvr] abstract class CameraUpdater {

  /**
   * The update method is called by the render connector to sync the cameras.
   */
  def update()
}


/**
 * This CameraUpdater connects to cameras to render a mirror plane. The transformation
 * of the reference camera is mirrored at the mirror plane and written to the mirror camera.
 * Over and above the head position and eye separation is kept in sync.
 *
 * @author Stephan Rehfeld
 *
 * @param mirrorCamera The camera that is used to render the image of the surface. Values get written in this object.
 * @param referenceCamera The original camera that is used to render the scene. Value are read from this object.
 * @param mirrorPlane The mirror plane. The transformation of the mirror camera is calculated in relation to the mirror plane.
 */
private[jvr] class MirrorCameraUpdater( mirrorCamera : VRCameraNode, referenceCamera : VRCameraNode, mirrorPlane : SceneNode ) extends CameraUpdater {

  require( mirrorCamera != null, "The parameter 'mirrorCamera' must not be 'null'!" )
  require( referenceCamera != null, "The parameter 'referenceCamera' must not be 'null'!" )
  require( referenceCamera != null, "The parameter 'mirrorPlane' must not be 'null'!" )

  override def update() {
    mirrorCamera.setTransform( mirrorPlane.getTransform.mul( Transform.scale(1,1,-1).mul( mirrorPlane.getTransform.invert.mul( referenceCamera.getTransform ) ) ) )
    mirrorCamera.setHeadTransform( referenceCamera.getHeadTransform )
    mirrorCamera.setEyeSeparation( referenceCamera.getEyeSeparation )
  }

  override def toString = "This MirrorCameraUpdater keeps the camera " + " in sync with mirror camera " + mirrorCamera + " relative to the plane " + mirrorPlane + "."

}