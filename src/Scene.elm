module Scene exposing (..)

import Color exposing (Color)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)


viewportWidthHeightRatio =
    16.0 / 9.0


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    , texture : Texture
    }


uniforms : Float -> Texture -> Uniforms
uniforms theta texture =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            (Mat4.makeRotate (2 * theta) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 45 viewportWidthHeightRatio 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 1
    , texture = texture
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


squareMesh =
    let
        a =
            vec3 -1 -1 0

        b =
            vec3 -1 1 0

        c =
            vec3 1 1 0

        d =
            vec3 1 -1 0

        white =
            vec3 1 1 1

        vertex position =
            Vertex white position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]
            |> WebGL.triangles



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3, vcoord : Vec2 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;

        varying vec3 vcolor;
        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
            vcoord = (position.xy + vec2(1.0, 1.0)) * 0.5;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, vcoord : Vec2 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        uniform sampler2D texture;
        varying vec3 vcolor;
        varying vec2 vcoord;

        float css(float apex, float width, float value) {
          return smoothstep(apex - width, apex, value) - smoothstep(apex, apex + width, value);
        }

        float f(vec2 position) {
          return position.x;
        }

        void main() {

            float value = f(vcoord);

            vec3 c1 = vec3(0.0, 0.0, 1.0);
            vec3 c2 = vec3(1.0, 1.0, 1.0);
            vec3 c3 = vec3(0.0, 1.0, 1.0);
            vec3 c4 = vec3(1.0, 1.0, 0.5);

            vec3 c
              = c1 * css(0.00, 0.33, value)
              + c2 * css(0.33, 0.33, value)
              + c3 * css(0.66, 0.33, value)
              + c4 * css(1.00, 0.33, value)
              ;

            gl_FragColor = vec4(c, 1.0);
        }
    |]


entities : Texture -> Float -> List WebGL.Entity
entities texture time =
    let
        theta =
            sin (0.001 * time) * pi / 32
    in
        [ WebGL.entity
            vertexShader
            fragmentShader
            squareMesh
            (uniforms theta texture)
        ]
