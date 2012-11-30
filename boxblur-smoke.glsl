// Procedural Smoke
// https://glsl.heroku.com/e#5127.2

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

vec2 origin = vec2(0.0);
vec2 uv, pos, pmouse;
float aspect;

//---------------------------------------------------------
// UTILS
//---------------------------------------------------------

// source: http://stackoverflow.com/questions/4200224/random-noise-functions-for-glsl
float rand(in vec2 seed) {
  return fract(sin(dot(seed.xy,vec2(12.9898,78.233))) * 43758.5453);
}

float snoise(vec2 v);
float snoise(vec3 v);
float snoise01(vec2 v) {
    return (snoise(v) + 1.0) / 2.0;
}
float snoise01(vec3 v) {
    return (snoise(v) + 1.0) / 2.0;
}

float circle (vec2 center, float radius) {
    float dist = distance(center, pos);
    
    return dist < radius ? 1.0 : 0.0;
}

float rect (vec2 center, vec2 b) {
    vec2 bMin = center-b;
    vec2 bMax = center+b;
    return ( 
        pos.x > bMin.x && pos.y > bMin.y && pos.x < bMax.x && pos.y < bMax.y 
        ) ? 
        1.0 : 0.0;
}

//---------------------------------------------------------
// PROGRAM
//---------------------------------------------------------

// create smoke from blurring backbuffer
vec3 ghostSmoke() {
    float timeUVnoise0 = snoise(vec3(uv.x, uv.y, time));
    float timeUVnoise1 = snoise(vec3(uv.x, uv.y, -time));
    float timeUVnoise2 = snoise(vec3(uv.x, uv.y, 1.0-time));
    
    // smoke travel speed
    float speed = 0.002;
    
    // smoke travel direction
    vec2 dir = vec2(0.5, 1.0);
    dir.x += 0.5 * timeUVnoise1;
    dir.y += 0.5 * timeUVnoise2;
    
    
    // size of step between box blur samples
    float kernelStep = 0.002;
    kernelStep += 0.003 * (1.0+snoise(vec2(time, uv.y)))/2.0;
    kernelStep += 0.003 * timeUVnoise0;
    
    // box blur
    
    #define KERNEL_HALF_SIZE 2
    #define KERNEL_SIZE 5
    vec3 ave = vec3(0.0);
    for (int y=-KERNEL_HALF_SIZE; y<=KERNEL_HALF_SIZE; ++y)
    for (int x=-KERNEL_HALF_SIZE; x<=KERNEL_HALF_SIZE; ++x) {
        vec2 spoint = vec2(float(x), float(y))*kernelStep;
        spoint += dir*speed;
        
        spoint.y = -spoint.y;
        spoint.x /= aspect;
        ave += texture2D(backbuffer, uv+spoint).rgb;
    }
    ave /= float(KERNEL_SIZE*KERNEL_SIZE);
    
    return ( snoise01(vec3(-pos.x, pos.y, -time))*0.01 + 0.99 ) * ave;
}

void main( void ) {
    aspect = resolution.x/resolution.y;
    uv = ( gl_FragCoord.xy / resolution.xy );
    pos = (uv-0.5);
    pos.x *= aspect;
    
    pmouse = mouse-vec2(0.5);
    pmouse.x *= aspect;
    
    vec3 cout = vec3(0.0);
    
    
    // seed shape
    
    vec3 color = vec3(1.0);
    
    // circle
    //float radius = 0.01;    
    //cout += color * snoise01(vec3(-pos.x, -pos.y, -time)) * circle(pmouse, radius);
    
    // rectangle
    vec2 rectbound = vec2(0.05, 0.005);
    rectbound.x *= (sin(time*2.0)+1.0) / 2.0 + 0.25;
    cout += color * snoise01(vec3(-pos.x, -pos.y, -time)) * rect(pmouse, rectbound);
    
    
    // add smoke
    cout += ghostSmoke();
    
    // clear;
    //cout = vec3(0.0);
	
    gl_FragColor = vec4(cout, 1.0);
}














//---------------------------------------------------------
// NOISE FUNCTIONS
//---------------------------------------------------------

/*
 * Description : Array and textureless GLSL 2D/3D/4D simplex 
 *               noise functions.
 *      Author : Ian McEwan, Ashima Arts.
 *  Maintainer : ijm
 *     Lastmod : 20110822 (ijm)
 *     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
 *               Distributed under the MIT License. See LICENSE file.
 *               https://github.com/ashima/webgl-noise
 */ 

vec4 _mod289(vec4 x)
{
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 _mod289(vec3 x)
{
    return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 _mod289(vec2 x) 
{
    return x - floor(x * (1.0 / 289.0)) * 289.0;
}

float _mod289(float x)
{
    return x - floor(x * (1.0 / 289.0)) * 289.0;
}
  
vec4 _permute(vec4 x)
{
    return _mod289(((x*34.0)+1.0)*x);
}

vec3 _permute(vec3 x)
{
    return _mod289(((x*34.0)+1.0)*x);
}

float _permute(float x) 
{
    return _mod289(((x*34.0)+1.0)*x);
}

vec4 _taylorInvSqrt(vec4 r)
{
    return 1.79284291400159 - 0.85373472095314 * r;
}

float _taylorInvSqrt(float r)
{
    return 1.79284291400159 - 0.85373472095314 * r;
}

vec4 _grad4(float j, vec4 ip)
{
    const vec4 ones = vec4(1.0, 1.0, 1.0, -1.0);
    vec4 p,s;

    p.xyz = floor( fract (vec3(j) * ip.xyz) * 7.0) * ip.z - 1.0;
    p.w = 1.5 - dot(abs(p.xyz), ones.xyz);
    s = vec4(lessThan(p, vec4(0.0)));
    p.xyz = p.xyz + (s.xyz*2.0 - 1.0) * s.www; 

    return p;
}
  
/*
 * Implemented by Ian McEwan, Ashima Arts, and distributed under the MIT License.  {@link https://github.com/ashima/webgl-noise}
 */  
float snoise(vec2 v)
{
    const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                        0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                       -0.577350269189626,  // -1.0 + 2.0 * C.x
                        0.024390243902439); // 1.0 / 41.0
    // First corner
    vec2 i  = floor(v + dot(v, C.yy) );
    vec2 x0 = v -   i + dot(i, C.xx);

    // Other corners
    vec2 i1;
    //i1.x = step( x0.y, x0.x ); // x0.x > x0.y ? 1.0 : 0.0
    //i1.y = 1.0 - i1.x;
    i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    // x0 = x0 - 0.0 + 0.0 * C.xx ;
    // x1 = x0 - i1 + 1.0 * C.xx ;
    // x2 = x0 - 1.0 + 2.0 * C.xx ;
    vec4 x12 = x0.xyxy + C.xxzz;
    x12.xy -= i1;

    // Permutations
    i = _mod289(i); // Avoid truncation effects in permutation
    vec3 p = _permute( _permute( i.y + vec3(0.0, i1.y, 1.0 )) + i.x + vec3(0.0, i1.x, 1.0 ));

    vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
    m = m*m ;
    m = m*m ;

    // Gradients: 41 points uniformly over a line, mapped onto a diamond.
    // The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)
    vec3 x = 2.0 * fract(p * C.www) - 1.0;
    vec3 h = abs(x) - 0.5;
    vec3 ox = floor(x + 0.5);
    vec3 a0 = x - ox;

    // Normalise gradients implicitly by scaling m
    // Approximation of: m *= inversesqrt( a0*a0 + h*h );
    m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );

    // Compute final noise value at P
    vec3 g;
    g.x  = a0.x  * x0.x  + h.x  * x0.y;
    g.yz = a0.yz * x12.xz + h.yz * x12.yw;
    return 130.0 * dot(m, g);
}

float snoise(vec3 v)
{ 
    const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
    const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

    // First corner
    vec3 i  = floor(v + dot(v, C.yyy) );
    vec3 x0 =   v - i + dot(i, C.xxx) ;

    // Other corners
    vec3 g = step(x0.yzx, x0.xyz);
    vec3 l = 1.0 - g;
    vec3 i1 = min( g.xyz, l.zxy );
    vec3 i2 = max( g.xyz, l.zxy );

    //   x0 = x0 - 0.0 + 0.0 * C.xxx;
    //   x1 = x0 - i1  + 1.0 * C.xxx;
    //   x2 = x0 - i2  + 2.0 * C.xxx;
    //   x3 = x0 - 1.0 + 3.0 * C.xxx;
    vec3 x1 = x0 - i1 + C.xxx;
    vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
    vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

    // Permutations
    i = _mod289(i); 
    vec4 p = _permute( _permute( _permute( 
                i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
              + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
              + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

    // Gradients: 7x7 points over a square, mapped onto an octahedron.
    // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
    float n_ = 0.142857142857; // 1.0/7.0
    vec3  ns = n_ * D.wyz - D.xzx;

    vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

    vec4 x_ = floor(j * ns.z);
    vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

    vec4 x = x_ *ns.x + ns.yyyy;
    vec4 y = y_ *ns.x + ns.yyyy;
    vec4 h = 1.0 - abs(x) - abs(y);

    vec4 b0 = vec4( x.xy, y.xy );
    vec4 b1 = vec4( x.zw, y.zw );

    //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
    //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
    vec4 s0 = floor(b0)*2.0 + 1.0;
    vec4 s1 = floor(b1)*2.0 + 1.0;
    vec4 sh = -step(h, vec4(0.0));

    vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
    vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

    vec3 p0 = vec3(a0.xy,h.x);
    vec3 p1 = vec3(a0.zw,h.y);
    vec3 p2 = vec3(a1.xy,h.z);
    vec3 p3 = vec3(a1.zw,h.w);

    //Normalise gradients
    vec4 norm = _taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
    p0 *= norm.x;
    p1 *= norm.y;
    p2 *= norm.z;
    p3 *= norm.w;

    // Mix final noise value
    vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
    m = m * m;
    return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}
