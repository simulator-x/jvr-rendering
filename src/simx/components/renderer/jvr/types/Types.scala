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

package simx.components.renderer.jvr.types

import simx.core.ontology.{SVarDescription, Symbols}
import de.bht.jvr.core.Texture2D


object DiffuseColor extends SVarDescription[de.bht.jvr.util.Color, java.awt.Color](simx.core.ontology.types.DiffuseColor  createdBy new de.bht.jvr.util.Color(0,0,0))

object HeadTransform extends SVarDescription[de.bht.jvr.core.Transform, simplex3d.math.floatx.ConstMat4f]( simx.core.ontology.types.HeadTransform as Symbols.headTransform createdBy(new de.bht.jvr.core.Transform) )

object Interactions extends SVarDescription( simx.core.ontology.types.AnyList as Symbols.interactions )

object PostProcessingEffect extends SVarDescription[simx.components.renderer.jvr.PostProcessingEffect, simx.components.renderer.jvr.PostProcessingEffect]( simx.core.ontology.types.NullType as Symbols.postProcessingEffect createdBy(new simx.components.renderer.jvr.PostProcessingEffect) )

object Scale extends SVarDescription[de.bht.jvr.core.Transform, simplex3d.math.floatx.ConstMat4f]( simx.core.ontology.types.Scale as Symbols.scale createdBy(new de.bht.jvr.core.Transform) )
object ShaderEffect extends SVarDescription[simx.components.renderer.jvr.ShaderEffect, simx.components.renderer.jvr.ShaderEffect]( simx.core.ontology.types.NullType as Symbols.shaderEffect createdBy(new simx.components.renderer.jvr.ShaderEffect( "none" )) )
object ShowHeightMap extends SVarDescription( simx.core.ontology.types.Boolean as Symbols.showHeightMap )
object SpecularColor extends SVarDescription[de.bht.jvr.util.Color, java.awt.Color](simx.core.ontology.types.SpecularColor createdBy new de.bht.jvr.util.Color(0,0,0))

object Transformation extends SVarDescription[de.bht.jvr.core.Transform, simplex3d.math.floatx.ConstMat4f]( simx.core.ontology.types.HeadTransform as Symbols.transformation createdBy(new de.bht.jvr.core.Transform) )

object ViewPlatform extends SVarDescription[de.bht.jvr.core.Transform, simplex3d.math.floatx.ConstMat4f]( simx.core.ontology.types.HeadTransform as Symbols.viewPlatform createdBy(new de.bht.jvr.core.Transform) )
