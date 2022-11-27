
/** @type {HTMLCanvasElement} */
const canvas = window.canvas
const gl = canvas.getContext("webgl2")
const dpr = Math.max(1, window.devicePixelRatio)
/** @type {Map<string,PointerEvent>} */
const touches = new Map()

const vertexSource = `#version 300 es
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

in vec2 position;

void main(void) {
    gl_Position = vec4(position, 0., 1.);
}
`
const fragmentSource = `#version 300 es
/*********
* made by Matthias Hurrle (@atzedent)
*/
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

out vec4 fragColor;

uniform vec2 resolution;
uniform vec2 touch;
uniform float time;
uniform int pointerCount;

#define mouse (touch/resolution)
#define T time
#define S smoothstep

mat3 rotX(float a) {
	float c = cos(a), s = sin(a);

	return mat3(
		vec3(1, 0, 0),
		vec3(0, c,-s),
		vec3(0, s, c)
	);
}

mat3 rotY(float a) {
	float c = cos(a), s = sin(a);

	return mat3(
		vec3(c, 0, s),
		vec3(0, 1, 0),
		vec3(-s,0, c)
	);
}

float smin(float a, float b, float k) {
	float h = clamp(
		.5+.5*(b-a)/k,
		.0,
		1.
	);

	return mix(b,a,h)-k*h*(1.-h);
}

float box(vec3 p, vec3 s, float r) {
	p = abs(p)-s;

	return length(max(vec3(0), p))+
		min(max(max(p.x, p.y),p.z),.0)-r;
}

float oct(vec3 p, float s) {
	p = abs(p);

	return (p.x+p.y+p.z-s)*(1./sqrt(3.));
}

float rnd(float a) {
	return fract(sin(a*711.599)*756.839);
}

float curve(float a, float b) {
	a /= b;

	return mix(
		rnd(floor(a)),
		rnd(floor(a)+1.),
		pow(S(.0,1.,fract(a)),10.)
	);
}

float tick(float t, float e) {
	return floor(t)+pow(S(.0,1.,fract(t)),e);
}

float map(vec3 p) {
	return smin(
		box(p, vec3(1), .125),
		oct(p, 1.8+.25*sin(curve(T, .5))),
		.125
	);
}

vec3 dir(vec2 uv, vec3 ro, vec3 target, float zoom) {
	vec3 up = vec3(0,1,0),
	f = normalize(target-ro),
	r = normalize(cross(up, f)),
	u = cross(f, r),
	c = f*zoom,
	i = c+uv.x*r+uv.y*u,
	d = normalize(i);

	return d;
}

vec3 norm(vec3 p) {
	vec2 e = vec2(1e-3, 0);
	float d = map(p);
	vec3 n = d - vec3(
		map(p-e.xyy),
		map(p-e.yxy),
		map(p-e.yyx)
	);

	return normalize(n);
}

void main(void) {
	vec2 uv = (
		gl_FragCoord.xy -.5 * resolution.xy
	) / min(resolution.x, resolution.y);

	vec3 col = vec3(0),
	ro = vec3(0,2.*sin(T*.5)*curve(T*.2, .2),-4);
	ro *= rotY(
		acos(0.)*sin(tick(T,4.))*
		(-1.+2.*curve(
			10.*sin(T)*curve(rnd(uv.y/uv.x)*5.*sin(T), 5.), 4.))
		);

	if(pointerCount > 0) {
		ro = vec3(0,0,-4);
		ro *= rotX(mouse.y*acos(-1.)+1.);
		ro *= rotY(mouse.x*acos(-1.)*2.);
	}

	vec3 rd = dir(uv, ro, vec3(0), 1.),
	p = ro;

	float i = .0, side = 1., at = .0;
	for(; i<50.;i++){
		float d = map(p)*side;

		if(d<1e-2) {
			vec3 n = norm(p)*side,
			l = normalize(ro-vec3(sin(T),cos(T),0)),
			r = reflect(rd, n);

			if(dot(l,n)<.0) l = -l;

			vec3 h = normalize(l-r);

			float fres = pow(1.-max(.0, dot(-r, n)), 5.),
			diff = pow(max(.0, dot(l,n)), 4.),
			fog = S(.2,.8,i/80.);

			col += fog +
				mix(vec3(1,.5+.5*sin(tick(T, 5.)),0), vec3(1,1,0),fog*fres)
				* diff * fres * (
				.8*pow(max(.0, dot(h,n)), 12.) +
				.6*pow(max(.0, dot(r, n)), 28.)
			);

			side = -side;

			vec3 rdo = refract(rd, n, 1.+.45*side);
			if(dot(rdo,rdo) == .0) {
				rdo = reflect(r, n);
			}

			rd = rdo;

			d = 2e-1;
		}

		if(d>20.) break;

		p += rd*d;
		at += .01*(.01/d);
	}

	col += pow(S(.0, 1., at), 3.);
	col += S(.5, .85, max(.01, i/80.));

    fragColor = vec4(col, 1.);
}
`
let time
let buffer
let program
let touch
let resolution
let pointerCount
let vertices = []
let touching = false

function resize() {
    const { innerWidth: width, innerHeight: height } = window

    canvas.width = width * dpr
    canvas.height = height * dpr

    gl.viewport(0, 0, width * dpr, height * dpr)
}

function compile(shader, source) {
    gl.shaderSource(shader, source)
    gl.compileShader(shader)

    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error(gl.getShaderInfoLog(shader))
    }
}

function setup() {
    const vs = gl.createShader(gl.VERTEX_SHADER)
    const fs = gl.createShader(gl.FRAGMENT_SHADER)

    program = gl.createProgram()

    compile(vs, vertexSource)
    compile(fs, fragmentSource)

    gl.attachShader(program, vs)
    gl.attachShader(program, fs)
    gl.linkProgram(program)

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error(gl.getProgramInfoLog(program))
    }

    vertices = [-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 1.0]

    buffer = gl.createBuffer()

    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW)

    const position = gl.getAttribLocation(program, "position")

    gl.enableVertexAttribArray(position)
    gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0)

    time = gl.getUniformLocation(program, "time")
    touch = gl.getUniformLocation(program, "touch")
    pointerCount = gl.getUniformLocation(program, "pointerCount")
    resolution = gl.getUniformLocation(program, "resolution")
}

function draw(now) {
    gl.clearColor(0, 0, 0, 1)
    gl.clear(gl.COLOR_BUFFER_BIT)

    gl.useProgram(program)
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)

    gl.uniform1f(time, now * 0.001)
    gl.uniform2f(touch, ...getTouches())
    gl.uniform1i(pointerCount, touches.size)
    gl.uniform2f(resolution, canvas.width, canvas.height)
    gl.drawArrays(gl.TRIANGLES, 0, vertices.length * 0.5)
}

function getTouches() {
    if (!touches.size) {
        return [0, 0]
    }

    for (let [id, t] of touches) {
        const result = [dpr * t.clientX, dpr * (innerHeight - t.clientY)]

        return result
    }
}

function loop(now) {
    draw(now)
    requestAnimationFrame(loop)
}

function init() {
    setup()
    resize()
    loop(0)
}

document.body.onload = init
window.onresize = resize
canvas.onpointerdown = e => {
    touching = true
    touches.set(e.pointerId, e)
}
canvas.onpointermove = e => {
    if (!touching) return
    touches.set(e.pointerId, e)
}
canvas.onpointerup = e => {
    touching = false
    touches.clear()
}
canvas.onpointerout = e => {
    touching = false
    touches.clear()
}