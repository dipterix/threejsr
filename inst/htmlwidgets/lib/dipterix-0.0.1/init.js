window.THREEJSRCANVAS = (function(){

  var __canvas = {};

  var __closest = function(e, ar){
    var l = Infinity, pl = -1, g = Infinity, pg = -1, tmp;
    for(var ii = 0; ii < ar.length; ii++){
      if(ar[ii] < e){
        if(l > e - ar[ii]){
          l = e - ar[ii];
          pl = ii;
        }
      }else{
        if(g > ar[ii] - e){
          g = ar[ii] - e;
          pg = ii;
        }
      }
    }


    return({
      'which_less' : pl,
      'which_greater' : pg,
    });
  };

  function __decode_datauri(data){
    window.dd = data;
  }



  function register(id, container, width, height){
    var innerCanvas, camera, controls, scene, renderer, stats, axes_helper, geoms, show_visible, mouse,
        ani_start, fps, event_stack;

    function animate(){
			requestAnimationFrame( animate );

      /* isAnimation, isLoop, frame, fps;
      for(var eid in call_stack){
			  if(call_stack[eid] !== undefined){
			    call_stack[eid](frame, updateFrequency = fps,
			    loop = isLoop);
			  }
			}
      if(isAnimation){
  			frame++;
      }
      */
      // FPS = 20;
      var __time_elapsed = (new Date()) - ani_start,
          __frame = __time_elapsed / 1000 * fps,
          frame = Math.floor(__frame),
          delta = __frame - frame;


			controls.update();
			mouse.update();
			geoms.forEach(function(e){
			  if(e.userData.animation !== undefined){
			    e.userData.animation(frame, delta);
			  }
			});
			render();
		}
    function resize(width, height){
  	  camera.aspect = width / height;
  		camera.updateProjectionMatrix();
  		renderer.setSize( width, height );
  		controls.handleResize();
  		render();
  	}
  	function switch_controls(on = ['trackball']){
  		controls._active = on;
  	}
  	function render(){
  		renderer.render( scene, camera );

  		if(typeof(stats) === 'object' && typeof(stats.update) === 'function'){
  		  stats.update();
  		}
  	}
  	function set_stats(show = false){
  	  if(show){
  	    stats.domElement.style.display = 'block';
  	  }else{
  	    stats.domElement.style.display = 'none';
  	  }
  	}
    function init(width, height){
      if ( ! Detector.webgl ) Detector.addGetWebGLMessage();

      // Trackball, Orbit, Mouse controls
      controls = {};
      geoms = [];
      ani_start = new Date();
      fps = 20;

      // stores subset of geoms
      event_stack = {};
      // Default: clip, hover
      event_stack.clip = [];
      event_stack.hover = [];



      // Main canvas for 3D objects
      innerCanvas = document.createElement('div');
      innerCanvas.style.width = '100%';


      // Camera
      camera = new THREE.PerspectiveCamera( 45, width / height, 1, 10000 );
  		camera.position.z = 500;
  		camera.layers.enable(1);
  		camera.layers.enable(2);
  		camera.layers.enable(3);
  		camera.layers.enable(4);

  		// World
  		scene = new THREE.Scene();
  		scene.background = new THREE.Color( 0xefefef );

  		/* Add the camera and a light to the scene, linked into one object. */
      var light = new THREE.DirectionalLight( 0xefefef, 0.5 );
      light.position.set(0,0,-1);
      camera.add(light);  // Add light to the camera, so it will move with the camera.
                          // (Trackball rotation is implemented by rotating the camera object.)

      scene.add( camera );

  		scene.add( new THREE.AmbientLight( 0x808080 ) ); // soft white light

  		// renderer
  		renderer = new THREE.WebGLRenderer( { antialias: false } );
  		renderer.setPixelRatio( window.devicePixelRatio );
  		renderer.setSize( width, height );
  		innerCanvas.appendChild( renderer.domElement );

  		axes_helper = new THREE.AxesHelper( 500 );
  		axes_helper.visible = false;
      scene.add( axes_helper );

      container.appendChild( innerCanvas );

      // Stats
      show_visible = false;
      stats = new Stats();
      stats.domElement.style.display = 'none';
      stats.domElement.style.position = 'relative';
      stats.domElement.style.float = 'right';
      stats.domElement.style.marginBottom = '-100%';
      container.parentNode.insertBefore( stats.domElement, container );


      // Controls
      // Trackball controls
      var trackball = new THREE.TrackballControls( camera, innerCanvas );
      trackball.rotateSpeed = 3.0;
  		trackball.zoomSpeed = 1.2;
  		trackball.panSpeed = 0.8;
  		trackball.noZoom = false;
  		trackball.noPan = false;
  		trackball.staticMoving = true;
  		trackball.dynamicDampingFactor = 0.3;
  		trackball.enableKeys = false;
  		trackball.addEventListener( 'change', render );
  		trackball.enabled = true;
  		controls.trackball = trackball;

  		var orbit = new THREE.TrackballControls( camera, innerCanvas );
  		orbit.rotateSpeed = 1.0;
  		orbit.zoomSpeed = 1.2;
  		orbit.panSpeed = 0.8;
  		orbit.noZoom = false;
  		orbit.noPan = false;
  		orbit.staticMoving = true;
  		orbit.dynamicDampingFactor = 0.3;
  		orbit.enableKeys = false;
  		orbit.addEventListener( 'change', render );
  		orbit.enabled = false;
  		controls.orbit = orbit;

  		controls.handleResize = function(){
  		  controls.trackball.handleResize();
  		  controls.orbit.handleResize();
  		};

  		controls._active = ['trackball'];

  		controls.update = function(){
  		  controls._active.forEach(function(element){
  		    controls[element].update();
  		  });
  		};

  		// Mouse events
  		mouse = {};
  		mouse.__pointer = new THREE.Vector2();
      mouse.__raycaster = new THREE.Raycaster();
      mouse.__intersects = [];
      var v = new THREE.Vector3( 1, 0, 0 );
      mouse.__arrow_helper = new THREE.ArrowHelper(v, v, 50, 0xff0000, 2 );
      mouse.__stats = {
        'isDown' : false
      };
      mouse.events = {
        'show_helper' : function(items, status){
          if(items.length > 0){
            var from = items[0].point,
                direction = items[0].face.normal.normalize(),
                back = mouse.__raycaster.ray.direction.dot(direction) > 0;
            if(back){
              direction.x = -direction.x;
              direction.y = -direction.y;
              direction.z = -direction.z;
            }
            mouse.__arrow_helper.position.set(from.x, from.y, from.z);
            mouse.__arrow_helper.setDirection(direction);
            mouse.__arrow_helper.visible = true;
          }else{
            mouse.__arrow_helper.visible = false;
          }
        },
        'show_info' : function(items, status){
          if(status.isDown && items.length > 0){
            var obj = items[0].object,
                is_mesh = obj.isMesh || false;
            if(is_mesh && typeof(obj.userData.mesh_info) === 'function'){
              obj.userData.mesh_info();
            }

          }
        }
      };

      mouse.get_mouse = function(event){
        mouse.__pointer.x = ( event.offsetX / innerCanvas.clientWidth ) * 2 - 1;
        mouse.__pointer.y = - ( event.offsetY / innerCanvas.clientHeight ) * 2 + 1;
      };
  		mouse.update = function(all = false){
  		  mouse.__raycaster.setFromCamera( mouse.__pointer, camera );
  		  if(all){
  		    mouse.__intersects = mouse.__raycaster.intersectObjects( geoms );
  		  }else{
  		    mouse.__intersects = mouse.__raycaster.intersectObjects( event_stack.hover );
  		  }

  		  for(var key in mouse.events){
  		    if(typeof(mouse.events[key]) === 'function'){
  		      mouse.events[key](mouse.__intersects, mouse.__stats);
  		    }
  		  }
  		};

      scene.add( mouse.__arrow_helper );

  		innerCanvas.addEventListener( 'mousemove', function(event){
         mouse.get_mouse(event);
      }, false );
      innerCanvas.addEventListener( 'mousedown', function(event){
         mouse.__stats.isDown = true;
         mouse.update(all = true);
      }, false );
      innerCanvas.addEventListener( 'mouseup', function(event){
         mouse.__stats.isDown = false;
         mouse.update();
      }, false );


      // All finished, render
      render();
    }

    function set_fps(new_fps){
      fps = new_fps;
    }

    function get_canvas(){
      return({
        'camera' : camera,
        'controls' : controls,
        'scene' : scene,
        'renderer' : renderer,
        'axes_helper' : axes_helper,
        'geoms' : geoms,
        'init' : init,
        'resize' : resize,
        'switch_controls' : switch_controls,
        'add_mesh' : add_mesh,
        'set_stats' : set_stats,
        'mouse_event' : mouse_event,
        'mouse' : mouse,
        'mesh_event' : mesh_event,
        'ani_start' : ani_start,
        'set_fps' : set_fps,
        'clear_all' : clear_all
      });
    }

    function clear_all(){
      for(var i in geoms){
        scene.remove(geoms[i]);
      }
      geoms.length = 0;  // remove all elements from the array
    }

    function add_mesh(mesh_type, mesh_name, geom_args,
                      position = [0,0,0], transform = undefined,
                      layer = 1, mesh_info = '',
                      enabled_events = ['clip', 'hover']){
      var geom_func = THREEJSRGEOMS[mesh_type],
          mesh_obj,
          geom_obj;

      if(typeof(geom_func) === 'function'){
        geom_obj = geom_func(geom_args);

        if(transform !== undefined){
          var mat = new THREE.Matrix4(),
              m = transform;

          mat.set(
            m[0][0], m[0][1], m[0][2], m[0][3],
            m[1][0], m[1][1], m[1][2], m[1][3],
            m[2][0], m[2][1], m[2][2], m[2][3],
            m[3][0], m[3][1], m[3][2], m[3][3]
          );

          geom_obj.geom.applyMatrix(mat);
        }

        mesh_obj = new THREE.Mesh( geom_obj.geom , new THREE.MeshLambertMaterial() );
        window.mm = mesh_obj;
        mesh_obj.layers.set(layer);
        mesh_obj.position.set(position[0], position[1], position[2]);
        mesh_obj.name = mesh_name;
        mesh_obj.userData.set_data_texture = geom_obj.set_data_texture;
        mesh_obj.userData.update_data_texture = geom_obj.update_data_texture;
        mesh_obj.userData.mesh_info = mesh_info;
        mesh_obj.userData.set_position = function(pos){
          mesh_obj.position.set(pos.x, pos.y, pos.z);
          if( mesh_obj.userData.position !== undefined ){
            mesh_obj.userData.position(pos);
          }
        };
        geoms.push(mesh_obj);
        if(typeof(enabled_events) !== 'object'){
          enabled_events = [enabled_events];
        }
        enabled_events.forEach(function(e){
          if(event_stack[e] === 'undefined'){
            event_stack[e] = [];
          }
          event_stack[e].push(mesh_obj);
        });
        scene.add(mesh_obj);
        return(mesh_obj);
      }else{
        console.log('Cannot find type '+ geom_func);
      }
    }

    // event_type can be one of the following
    // 1. animation
    // 2. position
    function mesh_event(mesh_name, event_type, data, sub_type = 'z'){
      var mesh = geoms.filter(g => g.name == mesh_name);
      if(mesh.length > 0){

        window.mm = data;

        mesh.forEach(function(e){

          var keys = Object.keys(data),
              max_len = keys.length,
              a, b;

          e.userData.set_data_texture( mesh = e, data = data,
                        max_anisotropy = renderer.capabilities.getMaxAnisotropy() );
          // e.material = new THREE.MeshBasicMaterial({ 'map' : map, 'side' : THREE.DoubleSide });

          switch (event_type) {
            case 'position':
              // dataTexture = new THREE.DataTexture( data, width, height, THREE.RGBFormat );

              var num = keys.map(Number);
              e.userData[event_type] = function(pos){

                var key = pos[sub_type],
                    ind = __closest(key, num),
                    key1, key2, a = 1, b = 1;

                if(ind.which_less > -1 && ind.which_greater > -1){
                  key1 = keys[ind.which_less];
                  key2 = keys[ind.which_greater];
                  a = (num[ind.which_greater] - key) / (num[ind.which_greater] - num[ind.which_less]);
                  b = 1 - a;
                }else if(ind.which_less > -1){
                  key1 = keys[ind.which_less];
                }else if(ind.which_greater > -1){
                  key1 = keys[ind.which_greater];
                }

                e.userData.update_data_texture(key1, key2, a, b);
                // e.userData.set_position(new THREE.Vector3().copy(e.position));
              };
              break;

            case 'animation':
              e.userData[event_type] = function(frame, delta){
                if(sub_type == 'loop'){
                  frame = frame % max_len;
                }
                if(frame < max_len){
                  e.userData.update_data_texture(
                    key1 = frame, key2 = frame + 1, a = 1 - delta, b = delta
                  );
                }
              };
              break;


            default:
              // code
          }

        });
      }
    }

    function mouse_event(et, fn){
      if(typeof(fn) === 'function'){
        mouse.events[et] = fn;
      }else{
        mouse.events[et] = undefined;
      }
    }









    init(width, height);
    animate();
    __canvas[id] = get_canvas;
    return(__canvas[id]());
  }


  return({
    'register' : register,
    'canvas_list' : __canvas,
    'uri_decode' : __decode_datauri
  });
})();




// register shiny messages
/* Start Comment
Shiny.addCustomMessageHandler("threejsrmessage",
  function(message) {
    var id = message.outputId,
        canvas = THREEJSRCANVAS.canvas_list[id];
    if(canvas === 'function'){
      canvas().receive_message(message);
    }
  }
);
/* End Comment */
