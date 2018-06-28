window.THREEJSRGEOMS = (function(){

  function freemesh(init_args){
    var g = new THREE.BufferGeometry();


    g.setIndex( init_args.faces );
    g.addAttribute( 'position', new THREE.Float32BufferAttribute( init_args.vertices, 3 ) );
		g.addAttribute( 'normal', new THREE.Float32BufferAttribute( init_args.normals, 3 ) );
		g.addAttribute( 'color', new THREE.Float32BufferAttribute( init_args.colors, 3 ) );
		g.computeVertexNormals();
		window.gg =g;

    // Tricky here, customized data will be textured for each vertices
    var __mesh,
        __dat,
        __width = 8;
    var set_data_texture = function( mesh, data, max_anisotropy = 1 ){
      /*
      We don't set texture to customized mesh data. Instead, we set vertex colors
      */
      __dat = data;
      __mesh = mesh;
			return(null);
    };

    var update_data_texture = function( key, key_2, a, b, update_texture = true ){
      var col = __dat[key],
          col_2 = __dat[key_2], c1,c2,c3;
      if(col === undefined && col_2 === undefined){
        return(null);
      }

      if(col === undefined){
        col = col_2;
        a = b = 0.5;
      }

      if(col_2 === undefined){
        col_2 = col;
        a = b = 0.5;
      }

      a = a / (a + b);
      b = 1-a;

      // what's the width of texture?
      /*for(var i = 0; i < (__width*__width * 3); i++){
        __cols[i] = col[i] * a + col_2[i] * b;
      }

      if(update_texture){
        __dataTexture.needsUpdate=true;
      }*/

    };

    return({
      'geom' : g,
      'set_data_texture' : set_data_texture,
      'update_data_texture' : update_data_texture
    });
  }



  function sphere(init_args){
    var radius=init_args.radius,
        widthSegments=init_args.widthSegments,
        heightSegments=init_args.heightSegments,
        phiStart=init_args.phiStart,
        phiLength=init_args.phiLength,
        thetaStart=init_args.thetaStart,
        thetaLength=init_args.thetaLength;
    var g = new THREE.SphereBufferGeometry(

      radius = (radius === undefined? 50 : radius),
      widthSegments = (widthSegments === undefined? 10 : widthSegments),
      heightSegments = (widthSegments === undefined? 6 : heightSegments),
      phiStart = (phiStart === undefined? 0 : phiStart),
      phiLength = (phiLength === undefined? 6.28318530717959 : phiLength),
      thetaStart = (thetaStart === undefined? 0 : thetaStart),
      thetaLength = (thetaLength === undefined? 3.1415926535 : thetaLength)

    );

    var __dat,
        __dataTexture,
        __width = 8,
        __cols = new Uint8Array( 3 * __width * __width );
    var set_data_texture = function( mesh, data, max_anisotropy = 1 ){
      /*
      * For sphere object, data will be: [key: val] - val is
      * single color - new Uint8Array( 3 );
      */
      __dat = data;

			__dataTexture = new THREE.DataTexture(
			      __cols , __width, __width,
			      THREE.RGBFormat,
			      THREE.UnsignedByteType,
			      THREE.UVMapping,
			      anisotropy = 1);

			mesh.material = new THREE.MeshBasicMaterial({ 'map' : __dataTexture });
			return(__dataTexture);
    };

    var update_data_texture = function( key, key_2, a, b, update_texture = true ){
      var col = __dat[key],
          col_2 = __dat[key_2], c;
      if(col === undefined && col_2 === undefined){
        return(null);
      }

      if(col === undefined){
        col = col_2;
        a = b = 0.5;
      }

      if(col_2 === undefined){
        col_2 = col;
        a = b = 0.5;
      }

      c = [
        col[0] * a + col_2[0] * b,
        col[1] * a + col_2[1] * b,
        col[2] * a + col_2[2] * b
      ];

      for(var i = 0; i<(__width*__width); i++){
        __cols[0 + 3*i] = c[0];
        __cols[1 + 3*i] = c[1];
        __cols[2 + 3*i] = c[2];
      }

      if(update_texture){
        __dataTexture.needsUpdate=true;
      }

    };

    return({
      'geom' : g,
      'set_data_texture' : set_data_texture,
      'update_data_texture' : update_data_texture
    });
  }


  function plane(init_args){
    init_args.widthSegments = init_args.widthSegments || 1;
    init_args.heightSegments = init_args.heightSegments || 1;
    var g = new THREE.PlaneBufferGeometry(
      width = init_args.width,
      height = init_args.height,
      widthSegments = init_args.widthSegments,
      heightSegments = init_args.heightSegments
    );

    var __dat,
        __dataTexture,
        __width,// = Math.pow(2, Math.floor(Math.log(Math.min(init_args.width, init_args.height)) / Math.log(2))),
        __cols;// = new Uint8Array( 3 * __width * __width );

    var set_data_texture = function( mesh, data, max_anisotropy = 1 ){
      /*
      * For plane object, data will be: [key: val] - val is
      * image - new Uint8Array( 3 x width x height );
      */
      __dat = data;
      __width = Math.floor(Math.sqrt(__dat[Object.keys(__dat)[0]].length / 3));
      console.log(__width);
      __cols = new Uint8Array( 3 * __width * __width );

			__dataTexture = new THREE.DataTexture(
			      __cols , __width, __width,
			      THREE.RGBFormat,
			      THREE.UnsignedByteType,
			      THREE.UVMapping,
			      anisotropy = max_anisotropy
			);

			__dataTexture.wrapS = __dataTexture.wrapT = THREE.ClampToEdgeWrapping;

			mesh.material = new THREE.MeshBasicMaterial({ 'map' : __dataTexture, 'side' : THREE.DoubleSide });
			return(__dataTexture);
    };

    var update_data_texture = function( key, key_2, a, b, update_texture = true ){
      var col = __dat[key],
          col_2 = __dat[key_2], c1,c2,c3;
      if(col === undefined && col_2 === undefined){
        return(null);
      }

      if(col === undefined){
        col = col_2;
        a = b = 0.5;
      }

      if(col_2 === undefined){
        col_2 = col;
        a = b = 0.5;
      }

      a = a / (a + b);
      b = 1-a;

      // what's the width of texture?
      for(var i = 0; i < (__width*__width * 3); i++){
        __cols[i] = col[i] * a + col_2[i] * b;
      }

      if(update_texture){
        __dataTexture.needsUpdate=true;
      }

    };

    return({
      'geom' : g,
      'set_data_texture' : set_data_texture,
      'update_data_texture' : update_data_texture
    });
  }


  return({
    'sphere' : sphere,
    'plane' : plane,
    'freemesh' : freemesh
  });
})();
