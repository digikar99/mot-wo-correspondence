
function ts() {
    return String(parseInt(Date.now()/1000));
}

const bg_color = "black";
const fg_color = "white";
const cross_color = "grey";
const hl_color = "yellow";
const session_id = prompt("Please enter a (psuedo)name", ts());
const canvas_size = 720;
const mot_canvas = document.getElementById("mot-canvas");
const ctx = mot_canvas.getContext("2d");
const POINT_SIZE=5;
// const refresh_rate = 120; // Hz
// const refresh_rate = 10; // Hz
// const refresh_rate = 20; // Hz
// const refresh_rate = 30; // Hz
// const refresh_rate = 40; // Hz
const refresh_rate = 60; // Hz
const ou_updates_per_second = 20; // Hz
const ou_updates_per_refresh = ou_updates_per_second / refresh_rate;
const refreshes_per_ou_update = refresh_rate / ou_updates_per_second;
const time_per_time_step = 1000/refresh_rate; // ms

const instructions_div = document.getElementById("instructions");
const points_div = document.getElementById("points");
var points = 0;

function update_instructions(suffix=""){
    instructions_div.innerHTML
        = (is_practice_trial()
           ? `<p>Practice Trial ${trial_idx+1} of ${num_practice_trials}</p>` + instructions : "")
        + suffix;
}

// const vh = document.getElementsByTagName("body")[0].clientHeight/100;
// const vw = document.getElementsByTagName("body")[0].clientWidth/100;

const vh = window.screen.height/100;
const vw = window.screen.width/100;

document.getElementById("mot-container").style.width  = `${canvas_size}px`;
document.getElementById("mot-container").style.height = `${canvas_size}px`;

mot_canvas.style.width  = `${canvas_size}px`;
mot_canvas.style.height = `${canvas_size}px`;
mot_canvas.style.marginTop = `${50*vh - canvas_size/2}px`;
mot_canvas.width  = String(canvas_size);
mot_canvas.height = String(canvas_size);

const begin_trial_button_div = document.getElementById("begin-trial-button");
const next_trial_button_div = document.getElementById("next-trial-button");

begin_trial_button_div.style.top = `${canvas_size/3}px`;
next_trial_button_div.style.top  = `${canvas_size/3}px`;
instructions_div.style.top = `${canvas_size/3+60}px`;

begin_trial_button_div.style.left = `${75+canvas_size/2+50*vw}px`;
next_trial_button_div.style.left  = `${120+75+canvas_size/2+50*vw}px`;
instructions_div.style.left = `${45+canvas_size/2+50*vw}px`;


var collecting_responses = false;

function float_equal(a, b, err=1e-6){
    return Math.abs(a-b)<err;
}

// https://stackoverflow.com/questions/17883692/how-to-set-time-delay-in-javascript
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// async function sleep(ms){await _sleep(ms);}

// https://stackoverflow.com/a/60476586/8957330
function gaussian(mean, stddev) {
    var V1;
    var V2;
    var S;
    do{
        var U1 = Math.random();
        var U2 = Math.random();
        V1 = 2*U1-1;
        V2 = 2*U2-1;
        S = V1*V1+V2*V2;
    }while(S >= 1)
    if(S===0) return 0;
    return mean+stddev*(V1*Math.sqrt(-2*Math.log(S)/S));
};

/**
 * Shuffles array in place.
 * @param {Array} a items An array containing the items.
 * Reference: https://stackoverflow.com/questions/6274339/how-can-i-shuffle-an-array
 */
function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}

function get_all_trial_data(){
    let xhr = new XMLHttpRequest();
    xhr.open("GET", "all-trial-data.json", false);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send(null);

    let data = JSON.parse(xhr.responseText);
    shuffle(data);
    let practice_trials = data.slice(0,num_practice_trials);
    shuffle(data);

    return practice_trials.concat(data);

    return [{
        num_objects: 14,
        num_targets: 4,
        sigma:1.25,
        k: 0.0005,
        lm:0.9,
        num_time_steps:50
    }, {
        num_objects: 14,
        num_targets: 4,
        sigma:1.25,
        k: 0.0005,
        lm:0.9,
        num_time_steps:50
    }, {
        num_objects: 14,
        num_targets: 4,
        sigma:1.25,
        k: 0.0005,
        lm:0.9,
        num_time_steps:50
    }];
};

var trial_idx = 0;
const num_practice_trials = 10;
// const num_practice_trials = 2;
const all_trial_data = get_all_trial_data();
const num_trials = all_trial_data.length;

function is_practice_trial(){
    return (trial_idx < num_practice_trials);
}

function clear_canvas(){
    // ctx.fillStyle = "white";
    // ctx.fillRect(0, 0, 720, 720);
    // ctx.fill();

    ctx.fillStyle = bg_color;
    ctx.beginPath();
    ctx.rect(0, 0, canvas_size, canvas_size);
    ctx.fill();
    console.log("Display cleared!");

    ctx.font = "90px Arial";
    ctx.textAlign = "center";
    ctx.fillStyle = cross_color;
    ctx.fillText("+", canvas_size/2, canvas_size/2+30);
}

function draw_point(x, y, size, color){
    // console.log("draw point called", x, y);
    ctx.font = "90px Arial";
    ctx.textAlign = "center";
    ctx.fillStyle = cross_color;
    ctx.fillText("+", canvas_size/2, canvas_size/2+30);

    ctx.fillStyle = color;
    ctx.beginPath();
    ctx.arc(x, y, size, 0, 2*Math.PI);
    ctx.fill();
};

function add_label(x, y, label, color, opposite=false){
    var xoffset = null, yoffset = null;
    const {width, height} = mot_canvas;

    if (!opposite){
        if (x < 20) xoffset = 10;
        else xoffset = -10;

        if (y < 20) yoffset = 10;
        else yoffset = -10;
    }else{
        if (x > width-20) xoffset = -10;
        else xoffset = 10;

        if (y > height-20) yoffset = -10;
        else yoffset = 10;
    }

    ctx.font = "16px Liberation Sans";
    ctx.fillStyle = color;
    ctx.fillText(label, x+xoffset, y+yoffset, 30);
}

function move_point(initX, initY, finalX, finalY, size, id=false){
    // ctx.clearRect(initX-size, initY-size, initX+size, initY+size);
    // ctx.clearRect(initX-size, initY-size, initX+size, initY+size);
    if (id) draw_point(initX, initY, 1.5*size+20, bg_color);
    else draw_point(initX, initY, 1.5*size, bg_color);
    draw_point(finalX, finalY, size, fg_color);
};

function redraw_objects_cleanly(object_list, size){
    clear_canvas();
    for(var i=0; i<object_list.length; i++){
        const o = object_list[i];
        draw_point(o.i, o.j, size, fg_color);
    }
};

function get_remaining_responses(){

    if (!collecting_responses) return;

    const trial_data = all_trial_data[trial_idx];
    const {responses, num_targets} = trial_data;

    let remaining_responses = Array.from(Array(num_targets).keys()).map(i => i+1);
    for(let i=0; i<responses.length; i++)
        remaining_responses.splice(
            remaining_responses.indexOf(responses[i].response_id),
            1
        );

    return remaining_responses;
}

var selected_object_idx = null;
function may_be_collect_response(e){
    if (!collecting_responses) return;
    const x = e.offsetX;
    const y = e.offsetY;
    const trial_data = all_trial_data[trial_idx];
    const {num_targets, object_list, responses} = trial_data;

    console.log("You clicked at", x, y);

    // toggle selection of the last object

    if (selected_object_idx != null){
        const o = object_list[selected_object_idx];
        draw_point(o.i, o.j, POINT_SIZE, fg_color);
    }
    var min_dist = Infinity;
    var min_idx = -1;
    for(var i = 0; i<object_list.length; i++){
        const o = object_list[i];
        const x2 = o.i; const y2 = o.j;
        const dist = Math.sqrt((x-x2)**2 + (y-y2)**2);
        if (dist<min_dist && dist<POINT_SIZE){
            min_dist = dist;
            min_idx = i;
        }
    }
    if (min_idx != -1){
        const o = object_list[min_idx];
        if (o === selected_object_idx){
            selected_object_idx=null;
        }else{
            draw_point(o.i, o.j, POINT_SIZE, hl_color);
            selected_object_idx = min_idx;
        }
    }

    if (selected_object_idx == null) return;
    const remaining_responses = get_remaining_responses();
    update_instructions(`<p>Now press one of the number keys: ${remaining_responses}</p>`);
};

// get the ID of the object that was clicked
function may_be_get_object_id(e){
    if (!collecting_responses || selected_object_idx == null) return;
    const remaining_responses = get_remaining_responses();
    if (!remaining_responses.includes(parseInt(e.key))){
        update_instructions(
            `<p style="color:red;">You pressed ${e.key}. You should press one of the number keys: ${remaining_responses}</p>`
        );
    }else{
        const trial_data = all_trial_data[trial_idx];
        const {num_targets, object_list, responses} = trial_data;
        const o = object_list[selected_object_idx];
        add_label(o.i, o.j, e.key, hl_color);
        // store the response
        const response = {
            true_id: selected_object_idx+1,
            response_id: parseInt(e.key),
        };
        console.log(response);
        responses.push(response);
        selected_object_idx = null;
        update_instructions();
        if (num_targets == responses.length) {
            complete_collecting_responses(trial_data);
            trial_idx += 1;
        }
    }
    console.log(e, e.keyCode);
}

function complete_collecting_responses(trial_data){
    const {num_targets, object_list, responses} = trial_data;
    if (is_practice_trial()){
        for (let i=0; i<num_targets; i++){
            let o = object_list[i];
            draw_point(o.i, o.j, POINT_SIZE, "yellow");
            add_label(o.i, o.j, String(i+1), "yellow", true);
        }
        for (let i=0; i<num_targets; i++){
            let r = responses[i];
            if (r.response_id != r.true_id){
                // remember IDs shown to the user are 1 more than the actual IDs
                let o = object_list[r.true_id-1];
                add_label(o.i, o.j, String(r.response_id), "red");
                if (r.true_id-1<num_targets){
                    draw_point(o.i, o.j, POINT_SIZE, "green");
                    add_label(o.i, o.j, String(r.true_id), "yellow", true);
                }else{
                    draw_point(o.i, o.j, POINT_SIZE, "red");
                }

            }else{
                let o = object_list[r.response_id-1];
                draw_point(o.i, o.j, POINT_SIZE, "green");
                add_label(o.i, o.j, String(r.response_id), "green");
            };
        }
    }
    let more_points = 0;
    for (let i=0; i<num_targets; i++){
        let response = responses[i];
        if (response.response_id == response.true_id) more_points += 10;
    }
    collecting_responses = false;
    toggle_button_from_div(next_trial_button_div, enable=true);

    points += more_points;
    points_div.innerText = `Points: ${points}`;
}

var uop_count = 0; // debug variable
function update_object_position(trial_data, object, remaining_time){
    uop_count += 1;
    var {i, j, x, y, vx, vy, remx, remy} = object;
    const {k, lm, sigma} = trial_data;
    const width  = mot_canvas.width;
    const height = mot_canvas.height;
    var newvx, newvy;
    if (float_equal(Math.round(remaining_time), remaining_time)){
        newvx = - k * x + lm*vx + gaussian(0,sigma);
        newvy = - k * y + lm*vy + gaussian(0,sigma);
        // console.log(remaining_time, "updating vx", newvx, vx);
    }else{
        newvx = vx;
        newvy = vy;
    }

    var dx = newvx*ou_updates_per_refresh;
    var dy = newvy*ou_updates_per_refresh;

    // console.log(dx, dy);

    // remx += dx - Math.floor(dx);
    // remy += dy - Math.floor(dy);
    // dx = Math.floor(dx);
    // dy = Math.floor(dy);
    // if (remx > 1){ remx -= 1; dx += 1;}
    // // if (remx < -1){ remx += 1; dx -= 1;}
    // if (remy > 1){ remy -= 1; dy += 1;}
    // // if (remy < -1){ remy += 1; dy -= 1;}

    var vx_is_reset = true, vy_is_reset = true;
    if (i+dx < 0) dx = -i;
    else if (i+dx >= width) dx = width - i - 1;
    else vx_is_reset = false;

    if (j+dy < 0) dy = -j;
    else if (j+dy >= height) dy = height - j - 1;
    else vy_is_reset = false;

    const newx = x + dx;
    const newy = y + dy;
    const newi = i + dx;
    const newj = j + dy;

    if (vx_is_reset) newvx = dx + newvx*(Math.ceil(remaining_time) - remaining_time)/time_per_time_step;
    if (vy_is_reset) newvy = dy + newvy*(Math.ceil(remaining_time) - remaining_time)/time_per_time_step;
    // if (vx_is_reset) newvx = dx + newvx*(Math.ceil(remaining_time) - remaining_time);
    // if (vy_is_reset) newvy = dy + newvy*(Math.ceil(remaining_time) - remaining_time);

    object.vx = newvx; object.vy = newvy;
    object.i = newi; object.j = newj;
    object.x = newx; object.y = newy;
    object.remx = remx; object.remy = remy;
    object.histi.push(newi); object.histj.push(newj);
    if (newx < object.minx) object.minx = newx;
    if (newx > object.maxx) object.maxx = newx;
    if (newy < object.miny) object.miny = newy;
    if (newy > object.maxy) object.maxy = newy;

    move_point(i, j, newi, newj, POINT_SIZE);
};

function get_random_coordinates(maxX, maxY){
    return [Math.floor(Math.random() * maxX), Math.floor(Math.random() * maxY)];
};

function initial_object_list(num_objects){
    const width = mot_canvas.width;
    const height = mot_canvas.height;
    var object_list = [];
    for(var i=0; i<num_objects; i++){
        const [i, j] = get_random_coordinates(width, height);
        const x = i - width/2;
        const y = j - height/2;
        var object = {
            i:i, j:j, x:x, y:y, vx:0, vy:0, // basic parameters
            minx:x, maxx:x, miny:y, maxy:y, // for bug fixing
            remx:0, remy:0, // for carrying forward remaining pixels in frames
            // remx and remy were supposed to be used in update_object_position;
            // but currently, these are found to be redundant
            histi:[], histj:[],
        };
        object_list.push(object);
    }
    return object_list;
};

function next_time_step(trial_data, object_list, remaining_time){
    if (float_equal(remaining_time, 0)) {
        console.log("Trial ended");
        let total_xrange = 0;
        let total_yrange = 0;
        for(var i=0; i<object_list.length; i++){
            let o = object_list[i];
            let xrange = Math.abs(o.maxx - o.minx);
            let yrange = Math.abs(o.maxy - o.miny);
            console.log(i, "Range", xrange, yrange);
            total_xrange += xrange;
            total_yrange += yrange;
        }
        console.log("Total Range", total_xrange, total_yrange);
        collecting_responses = true;
        return;
    }

    for(var i=0; i<object_list.length; i++){
        update_object_position(trial_data, object_list[i], remaining_time);
    }
    sleep(time_per_time_step).then(
        () => next_time_step(trial_data, object_list, remaining_time-ou_updates_per_refresh)
    );
};

function reset_mot_trial(trial_data){
    const {num_objects, num_targets, num_time_steps} = trial_data;
    console.log(num_objects, num_targets);
    var object_list = initial_object_list(num_objects);
    console.log(object_list);
    clear_canvas();
    ctx.font = "16px Liberation Sans";
    for(var i=0; i<num_objects; i++){
        const object = object_list[i];
        if (i<num_targets){
            draw_point(object.i, object.j, POINT_SIZE, hl_color);
            add_label(object.i, object.j, String(i+1), hl_color);
        }else{
            draw_point(object.i, object.j, POINT_SIZE, fg_color);
        }
    }
    trial_data.object_list = object_list;
    trial_data.remaining_time_steps = num_time_steps;
}

function toggle_button_from_div(div, enable=null){
    if (enable == null){
        div.childNodes[0].disabled = !div.childNodes[0].disabled;
    }else{
        div.childNodes[0].disabled = !enable;
    }
}

const red = `<span style="color:red;">red</span>`;

// const instructions = `<p>There are multiple dots in the window on the left.
// Some of them have an ID, while others don't.</p>

// <p>On clicking 'Begin Trial', all the dots will turn black, the IDs will disappear, and the
// dots will begin to move. Your task is to keep a track of the objects that had an ID.</p>

// <p>The dots will move for some time. After they stop moving, you are required to indicate which of them
// were initially ${red}, along with the ID of each of the object. To do so, you will need to click on an object you think was ${red} at the start of the trial, and indicate its ID with keypress. Once you indicate your response, you will be able to start the 'Next Trial'.</p>

// `;

// Let's see if changing the instructions so that people focus on the IDs
// rather than the colors makes a difference in tracking vs ID performance :D.
const instructions = `<p>There are multiple dots in the window on the left.
Some of them have an ID, while others don't.</p>

<p>On clicking 'Begin Trial', the IDs will disappear, all the dots will turn black, and they
will also begin to move. Your task is to keep a track of which object with ID at the start of the trial had which ID throughout the trial.</p>

<p>The dots will move for some time. After they stop moving, you are required to indicate which dot with the ID had which ID. To do so, you will need to click on an object that you thought had an ID, at the start of the trial, and indicate its ID by pressing the appropriate number key on the keyboard. Once you indicate this for all the objects with IDs, you will be able to start the 'Next Trial'. Correct responses will be shown for the practice trials.</p>

<p>Throughout the trial, you are requested to focus on the fixation cross at the center of the screen.</p>`;

` For each correct response, you will receive 10 points. These points are updated on the left.

<p><em>Note: Participants in the top 5 percentile of the scores will receive 4 times as many coupons.</em></p>
`;

function prepare_next_trial(){
    next_trial_button_div.childNodes[0].disabled=true;
    if (trial_idx >= num_trials){
        instructions_div.innerHTML
            = "<p>Uploading data... Please wait... Do NOT close the tab/window. </p>";
        upload_data();
        return;
    }
    if (trial_idx == 0) document.documentElement.requestFullscreen();
    const trial_data = all_trial_data[trial_idx];
    trial_data.lm = parseFloat(trial_data.lm);
    trial_data.k  = parseFloat(trial_data.k);
    trial_data.sigma = parseFloat(trial_data.sigma);
    trial_data.responses = [];
    reset_mot_trial(trial_data);
    begin_trial_button_div.childNodes[0].disabled=false;
    if (trial_idx < num_practice_trials){
        update_instructions();
    }else{
        instructions_div.innerHTML = "";
        instructions_div.display = "None";
        next_trial_button_div.style.top = "360px";
        begin_trial_button_div.style.top = "360px";
    }
};

function begin_trial(){
    begin_trial_button_div.childNodes[0].disabled=true;
    const trial_data = all_trial_data[trial_idx];
    const {object_list, remaining_time_steps} = trial_data;
    redraw_objects_cleanly(object_list, POINT_SIZE);
    sleep(100).then(() => next_time_step(trial_data, object_list, remaining_time_steps-1));
}

function upload_data(){
    while (!window.navigator.onLine)
        window.confirm("It seems your internet connection has been lost. Please reconnect to the internet and press OK to retry uploading data.");
    const session_details = {
        session_id: session_id,

        refresh_rate: refresh_rate,
        ou_updates_per_second: ou_updates_per_second,
        ou_updates_per_refresh: ou_updates_per_refresh,
        time_per_time_step: 1000/refresh_rate,

        num_trials: num_trials,
        num_practice_trials: num_practice_trials,

        shape: [canvas_size, canvas_size],

        points: points
    };
    const json_data = {
        session_details: session_details,
        all_trial_data: all_trial_data
    };
    const uint8_compressed_data = pako.deflate(JSON.stringify(json_data));
    // const uint8_compressed_data = JSON.stringify(json_data);
    const blob = new Blob([uint8_compressed_data], {type: "application/octet-stream"});
    console.log(blob);
    const file = new File([blob], session_details.session_id + ".json.zlib");
    const formdata = new FormData();
    formdata.append("uint8-blob", blob);
    formdata.append("filename", session_details.session_id + ".json.zlib");
    // console.log(uint8_compressed_data);
    // This does not work for large arrays
    // const b64_compressed_data = btoa(String.fromCharCode.apply(null, uint8_compressed_data));
    // fetch("./save-data.php", {
    //     method: "POST",
    //     headers: {'Content-Type': 'application/json'},
    //     body: JSON.stringify({
    //         filename: session_details.session_id + ".json.gz",
    //         data: b64_compressed_data
    //     })
    // })
    fetch("./save-data.php", {
        method: "POST",
        // headers: {'File-Name': session_details.session_id + ".json.gz"},
        body: formdata
    }).then(response => {
    if (response.ok){
            instructions_div.innerHTML
                = "<p>Data has been uploaded. You may now close this tab/window.</p>";
            return;
        }
        window.confirm("Something went wrong. Retrying data upload...");
        upload_data();
    }).catch(err => {
        window.confirm("Error: " + err + "\nRetrying data upload...");
        upload_data();
    });
}

mot_canvas.addEventListener("click", may_be_collect_response);
next_trial_button_div.addEventListener("click", prepare_next_trial);
begin_trial_button_div.addEventListener("click", begin_trial);
document.addEventListener("keypress", may_be_get_object_id);
// prepare_next_trial();
// run_all_trials();

// draw_point(24,24);
// sleep(2000).then(() => move_point(24,24,48,48,1));


