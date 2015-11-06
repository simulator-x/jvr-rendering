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
import simx.core.entity.typeconversion.TypeInfo._
import simx.core.helper.Loggable
import de.bht.jvr.collada14.loader.ColladaLoader
import de.bht.jvr.core.{ShaderProgram, Texture2D, GroupNode, SceneNode}
import simx.core.svaractor.{SingletonActor, SVarActor}

import scala.reflect.ClassTag

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
  private val sceneNodes = collection.mutable.Map[File,SceneNode]()

  /**
   * Cached textures.
   */
  private val textures = collection.mutable.Map[File,Texture2D]()

  /**
   * Cached shader programs.
   */
  private val shaderPrograms = collection.mutable.Map[List[File],ShaderProgram]()

  private val loading = collection.mutable.Map[File, List[SceneNode => Any]]()

  private def getLoading(f : File) = {
    loading.getOrElse(f, Nil)
  }

  addHandler[AskColladaFile]{ msg =>
    val file = msg.f
    checkFile(file)

    sceneNodes.get(file) match {
      case Some(loaded) =>
        info("Already loaded scene element from file: {}", file.getName )
        provideAnswer( loaded )
      case None =>
        info("Loading scene element from file: {}", file.getName )
        loading get file match {
          case None =>
            loading.update(file, Nil)
            delayedReplyWith(asyncLoad(file, (f : File) => ColladaLoader.load(f))){
              loaded =>
                sceneNodes.update(file, loaded )
                getLoading(file).foreach(_.apply( loaded ))
                loaded
            }
          case Some(list) =>
            loading.update(file,  provideAnswer :: list )
            DelayedAnswer
        }
    }
  }

  addHandler[AskTextureFile]{ msg =>
    val file = msg.f
    checkFile(file)

    textures.get(file) match {
      case Some(tex) =>
        info( "Already loaded texture: {}", file.getName )
        provideAnswer(tex)
      case None =>
        info( "Loading texture: {}", file.getName  )
        delayedReplyWith(asyncLoad[File, Texture2D](file, new Texture2D(_)) ){
          tex : Texture2D =>
            textures.update(file, tex)
            tex
        }
    }
  }

  addHandler[AskTextureFiles]{ msg =>
    val files = msg.fs.toSeq
    files.foreach(checkFile)
    if (files.forall(textures.keySet.contains))
      provideAnswer(files.map(textures.apply))
    else {
      val (loaded, toLoad) = files.partition(textures.keySet.contains)
      delayedReplyWith(asyncLoad(toLoad,  (_ : Seq[File]).map{ f => f -> new Texture2D(f) }  ) ) {
        newTextures =>
          newTextures.foreach( textures + _ )
          newTextures ++ loaded.map(f => f -> textures(f))
      }
    }
  }

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
        shaderPrograms.update(shader, loaded)
        provideAnswer(loaded)
    }
  }

  private def checkFile( file : File ){
    require( file != null, "The parameter 'file' must not be 'null'" )
    require( file.isFile, "The parameter 'file' must point to a file: " + file )
    require( file.exists, "The parameter 'file' must point to a existing file." )
  }

  protected def asyncLoad[T : DataTag, U : DataTag : ClassTag](file : T, loadFunc : T => U ) : (U => Any) => Unit =
    (handler : U => Any)  => ask[U](spawnActor(new LoaderActor(loadFunc)), LoadFile(file))(handler(_) )

  protected case class LoadFile[T](file : T)
  protected class LoaderActor[T : DataTag, U](loader : T => U) extends SVarActor{
    addHandler[LoadFile[T]]{ msg =>
      val retVal = loader( msg.file )
      context.stop(self)
      provideAnswer(retVal)
    }
  }
}
