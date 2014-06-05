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
import simx.core.helper.Loggable
import scala.reflect.runtime.universe.TypeTag
import de.bht.jvr.collada14.loader.ColladaLoader
import de.bht.jvr.core.{ShaderProgram, Texture2D, GroupNode, SceneNode}
import simx.core.svaractor.SVarActor
import simx.core.component.SingletonActor

// TODO: URGENT, Rewrite it!

/**
 * This is a resource manager for the jVR binding. It loads textures, scenes, nodes and shader programs.
 *
 * @author Stephan Rehfeld
 */
object ResourceManager extends SingletonActor(new ResourceManager, "jvr.ResourceManager"){
  // TODO: integrate resourcemanager instead
  def loadTexture( f : File) = new Texture2D(f)
}




case class AskColladaFile(f : File)
case class AskTextureFile(f : File)
case class AskTextureFiles(fs : Iterable[File])
case class AskShaderProgram( set : List[File])
case class AskLoadingFile(f : File)

class ResourceManager extends SVarActor with Loggable {

  /**
   * Cached scene graph nodes.
   */
  var sceneNodes: Map[File,SceneNode] = Map()

  private var loading = Map[File, List[SceneNode => Any]]()

  private def getLoading(f : File) = {
    loading.getOrElse(f, Nil)
  }

  addHandler[AskColladaFile]{ msg =>
    val file = msg.f
    checkFile(file)

    sceneNodes.get(file) match {
      case Some(loaded) =>
        info("Already loaded scene element from file: {}", file.getName )
        provideAnswer(new GroupNode().addChildNode( loaded ))
      case None =>
        info("Loading scene element from file: {}", file.getName )
        loading get file match {
          case None =>
            loading = loading.updated(file, Nil)
            delayedReplyWith[GroupNode](asyncLoad[File, SceneNode](ColladaLoader.load, file)){
            loaded  : GroupNode =>
              sceneNodes = sceneNodes + (file -> loaded )
              getLoading(file).foreach(_.apply(new GroupNode().addChildNode( loaded )))
              new GroupNode().addChildNode( loaded )
          }
          case Some(list) =>
            loading = loading.updated(file,  provideAnswer :: list )
            DelayedAnswer
        }
    }
  }


  /**
   * Cached textures.
   */
  var textures : Map[File,Texture2D] = Map()

  addHandler[AskTextureFile]{ msg =>
    val file = msg.f
    checkFile(file)

    textures.get(file) match {
      case Some(tex) =>
        info( "Already loaded texture: {}", file.getName )
        provideAnswer(tex)
      case None =>
        info( "Loading texture: {}", file.getName  )
        delayedReplyWith[Texture2D](asyncLoad[File, Texture2D]( new Texture2D(_), file) ){
          tex : Texture2D =>
            textures = textures + (file -> tex)
            tex
        }
    }
  }

  addHandler[AskTextureFiles]{ msg =>
    val files = msg.fs
    files.foreach(checkFile)
    if (files.forall(textures.keySet.contains))
      provideAnswer(files.map(textures.apply))
    else delayedReplyWith[Seq[(File, Texture2D)]](asyncLoad[Iterable[File], Seq[(File, Texture2D)]]( { _.map{ f => f -> textures.getOrElse(f, new Texture2D(f)) }.toSeq }, files ) ){
      texes : Seq[(File, Texture2D)] =>
        texes.map{ tuple => textures = textures + (tuple._1 -> tuple._2) }
        texes
    }
  }


  /**
   * Cached shader programs.
   */
  var shaderPrograms: Map[List[File],ShaderProgram] = Map()


  addHandler[AskShaderProgram]{ msg =>
    val shader = msg.set
    require( shader != null, "The parameter 'shader' must not be 'null'" )
    shader.foreach( checkFile )

    shaderPrograms.get( shader ) match {
      case Some(retVal) =>
        provideAnswer(retVal)
      case None =>
        info( "Loading shader program: {} and {}", shader.head.getName, shader.tail.head.getName  )
        val loaded = new ShaderProgram( shader.toArray : _* )
        shaderPrograms = shaderPrograms + (shader -> loaded)
        provideAnswer(loaded)
    }
  }

  private def checkFile( file : File ){
    require( file != null, "The parameter 'file' must not be 'null'" )
    require( file.isFile, "The parameter 'file' must point to a file: " + file )
    require( file.exists, "The parameter 'file' must point to a existing file." )
  }

  protected def asyncLoad[T : TypeTag, U]( loadFunc : T => U , file : T) =
    ask(createActor(new LoaderActor(loadFunc))(a => {})(), LoadFile(file)) _

  protected case class LoadFile[T](file : T)
  protected class LoaderActor[T : TypeTag, U](loader : T => U) extends SVarActor{
    addHandler[LoadFile[T]]{ msg =>
      val retVal = loader( msg.file )
      context.stop(self)
      provideAnswer(retVal)
    }
  }
}
