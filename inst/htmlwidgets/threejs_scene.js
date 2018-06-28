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

    // Get DOM elements
    var $el = $(el),
        $side_pane = null,
        $info_pane = null,
        $ctrl_pane = null,
        eid = el.id,
        canvas = THREEJSRCANVAS.register(id = eid, el, width, height);

    // Add data gui
    var gui = new dat.GUI({ autoPlace: false }),
        gui_position = null,
        gui_appended = false;

    window.cc = canvas;


    return {
      // "find", "renderError", "clearError", "sizing", "name", "type", "initialize", "renderValue", "resize"

      renderValue: function(x) {
        window.x = x;

        canvas.clear_all();

        if($side_pane === null){
          $el.parent().prepend(x.sidebar);
          $side_pane = $el.siblings('.threejs-scene-aside'),
          $info_pane = $side_pane.find('.threejs-scene-info').last(),
          $ctrl_pane = $side_pane.find('.threejs-scene-control').last(),
          // Set DOM elements
          $side_pane.height(height);
        }

        x.geoms.forEach(function(e){
          var mesh = canvas.add_mesh(
            e.mesh_type, e.mesh_name, e.geom_args,
            e.position, e.transform, e.layer,
            mesh_info = function(){
              $info_pane.html(e.mesh_info);
            },
            enabled_events = e.enabled_events
          );
          window.me = mesh;
          for(var event_type in e.events){
            canvas.mesh_event(e.mesh_name, event_type,
                e.events[event_type].data, sub_type = e.events[event_type].sub_type);
          }
          Object.keys(e.controls).forEach(function(control_type){
            var params = e.controls[control_type];
            switch (control_type) {
              case 'position':
                if(gui_position === null){
                  gui_position = gui.addFolder('Position');
                  gui_position.open();
                }
                gui_position.add(
                  e.controls[control_type], params.label, params.min, params.max
                ).step(params.step).onChange(function(value) {
                  var pos = mesh.position;
                  pos[params.axis] = value;
                  mesh.userData.set_position(pos);
                });
                break;

              default:
                // code
            }

            if(Object.keys(e.controls[control_type]).length > 1 && !gui_appended){
              gui_appended = true;
              $ctrl_pane.append(gui.domElement);
              $ctrl_pane.removeClass('hidden');
            }
          });

          // Sadly, we need to set positions here again
          mesh.userData.set_position({
            'x' : e.position[0],
            'y' : e.position[1],
            'z' : e.position[2]
          });
        });

        canvas.switch_controls([x.control]);

        canvas.set_stats(x.show_stats);
        canvas.set_fps(x.fps);
      },

      resize: function(width, height) {
        if($side_pane !== null){
          $side_pane.height(height);
        }
        canvas.resize(width, height);
      },

      s: this
    };
  }
});
