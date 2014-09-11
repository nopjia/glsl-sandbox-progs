// Procedural Noise Emittor
// http://glslsandbox.com/e#19829.5

// By Nop Jiarathanakul
// http://www.iamnop.com/

#ifdef GL_ES
precision highp float;
#endif

//---------------------------------------------------------
// MACROS
//---------------------------------------------------------

#define EPS       0.0001
#define PI        3.14159265
#define TWOPI     6.28318531
#define HALFPI    1.57079633
#define ROOTTHREE 1.73205081

#define EQUALS(A,B) ( abs((A)-(B)) < EPS )
#define EQUALSZERO(A) ( ((A)<EPS) && ((A)>-EPS) )

//---------------------------------------------------------
// UNIFORMS
//---------------------------------------------------------

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;
uniform sampler2D backbuffer;

//---------------------------------------------------------
// GLOBALS
//---------------------------------------------------------

const vec2 ORIGIN = vec2(0.0);
vec2 uv, pos, pmouse;
float aspect;

//---------------------------------------------------------
// UTILS
//---------------------------------------------------------

// source: http://stackoverflow.com/questions/4200224/random-noise-functions-for-glsl
float rand(in vec2 seed) {
  return fract(sin(dot(seed.xy,vec2(12.9898,78.233))) * 43758.5453);
}

float circle (vec2 center, float radius) {
    float dist = distance(center, pos);

    return dist < radius ? 1.0 : 0.0;
}

float rect (vec2 center, vec2 b) {
    vec2 bMin = center-b;
    vec2 bMax = center+b;
    return (pos.x > bMin.x &&
        pos.y > bMin.y &&
        pos.x < bMax.x &&
        pos.y < bMax.y) ?
        1.0 : 0.0;
}

float snoise(vec3 uv, float res) {
    const vec3 s = vec3(1e0, 1e2, 1e3);

    uv *= res;

    vec3 uv0 = floor(mod(uv, res))*s;
    vec3 uv1 = floor(mod(uv+vec3(1.), res))*s;

    vec3 f = fract(uv); f = f*f*(3.0-2.0*f);

    vec4 v = vec4(uv0.x+uv0.y+uv0.z, uv1.x+uv0.y+uv0.z,
                  uv0.x+uv1.y+uv0.z, uv1.x+uv1.y+uv0.z);

    vec4 r = fract(sin(v*1e-1)*1e3);
    float r0 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);

    r = fract(sin((v + uv1.z - uv0.z)*1e-1)*1e3);
    float r1 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);

    return mix(r0, r1, f.z)*2.-1.;
}

//---------------------------------------------------------
// PROGRAM
//---------------------------------------------------------

void calcGlobalVars() {
    aspect = resolution.x/resolution.y;
    uv = ( gl_FragCoord.xy / resolution.xy );
    pos = (uv-0.5);
    pos.x *= aspect;

    pmouse = mouse-vec2(0.5);
    pmouse.x *= aspect;
}

void main(void) {
    calcGlobalVars();

    // domain transform
    pos = vec2(atan(pos.y, pos.x), distance(ORIGIN, pos));

    float modulator = 0.0;

    // sin wave effect
    const float SIN_FREQ = 30.0;
    const float SIN_SPEED = 5.0;
    modulator += ( sin(SIN_FREQ*pos.y - SIN_SPEED*time) + 1.0) / 2.0;

    // noise effect
    const float NOISE_XFREQ = 1.0;
    const float NOISE_YFREQ = 2.0;
    const float NOISE_YSPEED = 0.5;
    const float NOISE_ZSPEED = 0.1;
    modulator += snoise(vec3(
        NOISE_XFREQ * pos.x/TWOPI,
        NOISE_YFREQ * pos.y - NOISE_YSPEED*time,
        NOISE_ZSPEED * time), 20.0);

    vec3 color = vec3(1.0);
    vec3 cout = color * modulator;

    gl_FragColor = vec4(cout, 1.0);
}