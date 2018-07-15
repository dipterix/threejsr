/* Wrap up the whole script within a function
+(function(){

})();
*/


// Override methods so that we have multiple support across platforms
window.requestAnimationFrame =
    window.requestAnimationFrame ||
    window.mozRequestAnimationFrame ||
    window.webkitRequestAnimationFrame ||
    window.msRequestAnimationFrame ||
    window.oRequestAnimationFrame ||
    function (callback) {
        setTimeout(function() { callback(Date.now()); },  1000/60);
    };

function is_in(e, li){
  var re = false;
  if(e !== undefined){
    re = li.indexOf(e) > -1;
  }
  return(re);
}


HTMLWidgets.widget({

  name: "threejs_scene",

  type: "output",

  factory: function(el, width, height) {
    // get full width if not shiny mode
    if(!HTMLWidgets.shinyMode){
      document.body.style.padding = 0;
      $(el).css({'height':'100vh'});
    }

    // Get DOM elements
    var $el = $(el),
        $side_pane;

    var __sidebar = `
    <div class="threejs-scene-aside " style="height: ` + height + 'px; margin-bottom: -' + height + `px;">
      <div class="row">
        <div class="col-sm-1">
          <div class="threejs-scene-control hidden"></div>
          <div style="threejs-scene-customized"></div>
          <div class="threejs-scene-info"></div>
        </div>
        <div class="col-sm-8"> </div>
        <div class="col-sm-3">
          <div style="float:right;">
            <div class="threejs-scene-sidecamera"></div>
          </div>
        </div>
      </div>
    </div>`;


    $el.prepend(__sidebar);
    $side_pane = $el.find('.threejs-scene-aside');

    var $info_pane = $side_pane.find('.threejs-scene-info').last(),
        $ctrl_pane = $side_pane.find('.threejs-scene-control').last(),
        $side_camr = $side_pane.find('.threejs-scene-sidecamera').last(),
        $side_cust = $side_pane.find('.threejs-scene-customized').last(),
        eid = el.id,
        canvas = THREEJSRCANVAS.register(id = eid, el, width, height);

    window.cc = canvas;

    // Add shiny callbacks
    var shiny_input_id;
    function threejsr_to_shiny(data){
      if(HTMLWidgets.shinyMode){
        if(typeof(shiny_input_id) !== 'string' || shiny_input_id === ''){
          shiny_input_id = eid + '_callback';
        }
        var re = {...data, '.__timestamp__.': new Date()};
        Shiny.onInputChange(shiny_input_id, re);
      }
    }


    return {
      // "find", "renderError", "clearError", "sizing", "name", "type", "initialize", "renderValue", "resize"

      renderValue: function(x) {
        window.x = x;

        // Add data gui
        var gui = new dat.GUI({ autoPlace: false }),
            gui_folders = {},
            gui_appended = false,
            max_keyframe = -1;



        if(!HTMLWidgets.shinyMode){
          window.dispatchEvent(new Event('resize'));
        }

        /* Clear previous elements
        The reason not to init a new canvas (which I think is what r-plotly is doing) is that
        we don't want to camera to be reset if same data pushed in. If you want to reset camera,
        I'll implement that later. Right now, you can also use shiny::uiOutput as a wrapper since that
        uiOutput will refresh everything inside it
        */

        // Clear canvas and renderers
        canvas.clear_all();
        // Clear gui controls
        $ctrl_pane.html('');
        // Add sidebars to canvas
        $side_cust.html(x.sidebar);
        // force change shiny callback
        // By default callback ID is shinyInputId + '_callback'. However this can be reset
        // So that you can have multiple callbacks in the runtime.
        shiny_input_id = x.callback_id;


        canvas.set_renderer_colors(
          new THREE.Color().fromArray(x.background_colors[0]),
          new THREE.Color().fromArray(x.background_colors[1])
        );


        x.geoms.forEach(function(e){
          e = JSON.parse(e);
          var mesh = canvas.add_mesh(
            e.mesh_type, e.mesh_name, e.geom_args,
            e.position, e.transform, e.layer,
            mesh_info = function(){
              $info_pane.html(e.mesh_info);  // extra_data = e.extra_data
              if(
                HTMLWidgets.shinyMode &&
                Object.keys(e.extra_data).length > 1 &&
                e.extra_data.text !== undefined
              ){
                $info_pane.append('<span> <a href="#" class="threejsr-shiny-callback">'+
                  e.extra_data.text+
                  '</a></span>'
                );
                // add listener to shiny callbacks
                $info_pane.find(".threejsr-shiny-callback").click(function(){
                  threejsr_to_shiny(e.extra_data);
                });
              }
            },
            clippers = e.clippers,
            clip_intersect = e.clip_intersect,
            is_clipper = e.is_clipper,
            hover_enabled = e.hover_enabled
          );





          // Add control UIs
          Object.keys(e.controls).forEach(function(folder_name){
            var params = e.controls[folder_name];
            if(gui_folders[folder_name] === undefined){
              gui_folders[folder_name] = gui.addFolder(folder_name);
              gui_folders[folder_name].open();
            }

            // for each of params, i.e. p, add controls
            params.forEach(function(p){
              mesh.userData.__params[p.name] = p;
              p.__values = {};
              p.__values[p.label] = p.initial;

              if(typeof(p.callback) === 'string'){
                mesh.userData.__funs[p.name] = eval('var __tmp='+p.callback+';__tmp;');
                mesh.userData.__funs[p.name](p.initial, mesh);
              }

              gui_folders[folder_name].add(
                  p.__values, p.label, p.min, p.max
                ).step(p.step).onChange(function(value) {
                  if(typeof(mesh.userData.__funs[p.name]) === 'function'){
                    mesh.userData.__funs[p.name](value, mesh = mesh);
                  }
                });

            });

          });


          // add mesh event if exists
          // Add controls
          Object.keys(e.events).forEach(function(event_type){
            e.events[event_type].forEach(function(args){
              canvas.mesh_event(
                  mesh_name = e.mesh_name,
                  event_type = event_type,
                  args = args
                );

              if(event_type === 'animation' && typeof(args.key_frames) === 'object'){
                var kf = args.key_frames.map(k => parseFloat(k));
                kf = kf.filter(k => !isNaN(k));
                max_keyframe = Math.max(...kf);
              }
            });
          });


        });


        canvas.switch_controls([x.control]);

        canvas.set_stats(x.show_stats);
        canvas.set_fps(x.fps);

        canvas.post_init();


        //--- Add sidebar cameras

        // Clear sidebar cameras
        $side_camr.html('');
        if(typeof(x.extra_cameras) === 'object' && x.extra_cameras !== null && (x.extra_cameras.length || 0) > 0){
          x.extra_cameras.forEach(function(args){
            canvas.side_camera(args.look_at, args);
          });

          canvas.set_side_renderer($side_camr[0]);
        }



        // Add dat gui

        if(max_keyframe > 0){
          // Now we have animation event(s)
          // add animation controllers
          if(gui_folders.Animation === undefined){
            gui_folders.Animation= gui.addFolder('Animation');
          }
          gui_folders.Animation.open();

          var ani_params = {
            'Play/Pause' : true,
            'Reset' : canvas.ani_reset,
            'Speed' : 0,
            'Key Frame' : 0
          };

          canvas.ani_maxkeyframe(max_keyframe);

          gui_folders.Animation.add(ani_params, 'Play/Pause').onChange(function(v){ canvas.ani_toggle(v); });
          gui_folders.Animation.add(ani_params, 'Reset');
          gui_folders.Animation.add(ani_params, 'Speed', -1, 2).step(0.01).onChange(function(v){
            var fps = Math.pow(10, v);
            canvas.set_fps(fps);
          });
          gui_folders.Animation.add(ani_params, 'Key Frame', 0, max_keyframe - 0.1).step(0.1)
          .onChange(function(v){
            canvas.ani_reset(v);
          });

          window.ggg = gui;
          // Add animation callbacks to update bar
          canvas.ani_callback(function(frame){
            ani_params['Key Frame'] = frame;
            // Iterate over all controllers
            for (var i in gui_folders.Animation.__controllers) {
              if(gui_folders.Animation.__controllers[i].property == 'Key Frame'){
                gui_folders.Animation.__controllers[i].updateDisplay();
              }
            }
            console.log(frame);
          }, true);



        }


        if(x.control_gui && !gui_appended){
          gui_appended = true;
          $ctrl_pane.append(gui.domElement);
          $ctrl_pane.removeClass('hidden');

        }else if(!x.control_gui){
          $ctrl_pane.addClass('hidden');
        }





      },

      resize: function(width, height) {
        if($side_pane !== null){
          $side_pane.height(height);
          $side_pane.css({
            'margin-bottom': '-' + height + 'px'
          });
        }

        canvas.resize(width, height);
      },

      s: this
    };
  }
});
