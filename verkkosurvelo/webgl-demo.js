// Vertex shader program
const vsSource = `
    attribute vec4 aVertexPosition;
    attribute vec4 aVertexColor;

    uniform mat4 uModelViewMatrix;
    uniform mat4 uProjectionMatrix;

    varying lowp vec4 vColor;

    void main() {
      gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
      vColor = aVertexColor;
    }
  `;

const fsSource = `
varying lowp vec4 vColor;

    void main() {
      gl_FragColor = vColor;
    }
  `;


function loadShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);

    if(!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
	let type_str = type == gl.VERTEX_SHADER ? "vertex": "fragment";
	alert (`${type_str} shader compilation fail ${gl.getShaderInfoLog(shader)}`);
	gl.deleteShader(shader);
	return null;
    }

    return shader;
}

function initShaderProgram(gl, vsSource, fsSource) {
    const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vsSource),
	  fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fsSource);

    const shaderProgram = gl.createProgram();
    gl.attachShader(shaderProgram, vertexShader);
    gl.attachShader(shaderProgram, fragmentShader);
    gl.linkProgram(shaderProgram);

    
  if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
    alert(
      `Unable to initialize the shader program: ${gl.getProgramInfoLog(
        shaderProgram,
      )}`,
    );
    return null;
  }

  return shaderProgram;

}


const positions = [1.0, 1.0, -1.0, // back face
		   -1.0, 1.0, -1.0,
		   1.0, -1.0, -1.0,
		   
		   1.0, -1.0, -1.0,
		   -1.0, 1.0, -1.0,
		   -1.0, -1.0, -1.0,

		   // top face
		   1.0, 1.0, -1.0,
		   -1.0, 1.0, -1.0,
		   -1.0, 1.0, 1.0,

		   -1.0, 1.0, 1.0,
		   1.0, 1.0, 1.0, 
		   1.0, 1.0, -1.0,

		   // front face?
		   1.0, 1.0, 1.0,
		   -1.0, 1.0, 1.0,
		   1.0, -1.0, 1.0,
		   

		   1.0, -1.0, 1.0,
		   -1.0, 1.0, 1.0,
		   -1.0, -1.0, 1.0,

		   -1.0, -1.0, 1.0,
		   -1.0, -1.0, -1.0,
		   1.0, -1.0, 1.0,

		   -1.0, -1.0, -1.0,
		   1.0, -1.0, -1.0,
		   1.0, -1.0, 1.0
		   
		   
		  ];

function initColorBuffer(gl) {
    const colors = [
		0.0,
	1.0,
	0.0,
	1.0,

	0.0,
	1.0,
	0.0,
	1.0,
	
	0.0,
	1.0,
	0.0,
	1.0,
	
	0.0,
	0.0,
	1.0,
	1.0,
	
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	0.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	0.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	0.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	0.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	0.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	0.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	0.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	0.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	1.0,
	0.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	1.0,
	0.0,
	0.0,
	1.0,
	1.0,
    ];

  const colorBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(colors), gl.STATIC_DRAW);

  return colorBuffer;
}


function initPositionBuffer(gl) {
    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    return positionBuffer;
}

function initBuffers(gl) {
    const positionBuffer = initPositionBuffer(gl);
    const colorBuffer = initColorBuffer(gl);
    return { position: positionBuffer,
	     color: colorBuffer}
}

function setColorAttribute(gl, buffers, programInfo) {
  const numComponents = 4;
  const type = gl.FLOAT;
  const normalize = false;
  const stride = 0;
  const offset = 0;
  gl.bindBuffer(gl.ARRAY_BUFFER, buffers.color);
  gl.vertexAttribPointer(
    programInfo.attribLocations.vertexColor,
    numComponents,
    type,
    normalize,
    stride,
    offset,
  );
  gl.enableVertexAttribArray(programInfo.attribLocations.vertexColor);
}


let to_rad = deg => deg * Math.PI / 180;

function drawScene(gl, programInfo, buffers) {
    gl.clearColor(0.0, 0.0, 0.0, 1.0); 
    gl.clearDepth(1.0); 
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
    
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    const fieldOfView = (45 * Math.PI) / 180; // in radians
    const aspect = gl.canvas.clientWidth / gl.canvas.clientHeight;
    const zNear = 0.1;
    const zFar = 100.0;
    const projectionMatrix = mat4.create();

    mat4.perspective(projectionMatrix, fieldOfView, aspect, zNear, zFar);

    const modelViewMatrix = mat4.create();
    mat4.translate(
	modelViewMatrix, 
	modelViewMatrix, 
	[-0.0, 0.0, -6.0],
    );

    mat4.rotate(
	modelViewMatrix,
	modelViewMatrix,
	to_rad(45),
	[0.0, 1.0, 0.0]); 

    setPositionAttribute(gl, buffers, programInfo);
    setColorAttribute(gl, buffers, programInfo);
    gl.useProgram(programInfo.program);

    // Set the shader uniforms
    gl.uniformMatrix4fv(
	programInfo.uniformLocations.projectionMatrix,
	false,
	projectionMatrix,
    );
    gl.uniformMatrix4fv(
	programInfo.uniformLocations.modelViewMatrix,
	false,
	modelViewMatrix,
    );


    const offset = 0;
    const vertexCount = positions.length / 3;
    gl.drawArrays(gl.TRIANGLES, offset, vertexCount);
}

function setPositionAttribute(gl, buffers, programInfo) {
    const numComponents = 3; 
    const type = gl.FLOAT; 
    const normalize = false; 
    const stride = 0; 
    const offset = 0;
    gl.bindBuffer(gl.ARRAY_BUFFER, buffers.position);
    gl.vertexAttribPointer(
	programInfo.attribLocations.vertexPosition,
	numComponents,
	type,
	normalize,
	stride,
	offset,
    );
    gl.enableVertexAttribArray(programInfo.attribLocations.vertexPosition);
}

function main(_) {
    const canvas = document.getElementById("glcanvas");
    const gl = canvas.getContext('webgl');

    if(!gl) {
	alert ("can't initialize gl");
	return;
    }

    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    const shaderProgram = initShaderProgram(gl, vsSource, fsSource);
    const programInfo = {
	program: shaderProgram,
	attribLocations: {
	    vertexPosition: gl.getAttribLocation(shaderProgram, "aVertexPosition"),
	    vertexColor: gl.getAttribLocation(shaderProgram, "aVertexColor")
	},
	uniformLocations: {
	    projectionMatrix: gl.getUniformLocation(shaderProgram, "uProjectionMatrix"),
	    modelViewMatrix: gl.getUniformLocation(shaderProgram, "uModelViewMatrix"),
	}};

    const buffers = initBuffers(gl);

    drawScene(gl, programInfo, buffers);
}


document.addEventListener("DOMContentLoaded", main)
