const pluginStorage = PluginStorageService();
const pluginDialogs = PluginDialogsService();

Assets = {
    doneIcon: '<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg"><g><line stroke-width="5" stroke="#000" id="svg_4" y2="24.27273" x2="13.18182" y1="16.09091" x1="7.54545" fill="none"/><line stroke="#000" stroke-width="5" id="svg_5" y2="11" x2="26.81818" y1="23.72727" x1="10.09091" fill="none"/></g></svg>'
}

$(document).ready(function(){
    const routineTemplate = '<div class="draghandle">' + 
                '<span class="title"></span>' +
                '<button class="delete-item">X</button></div>'+
                '<button class="done-btn {doneFlag}">' + Assets.doneIcon + '</button>'+
                '<div class="body">{title}</div>';

    function loadRoutines(testOnly=false) {
        pluginStorage.ReadLayoutValue('RoutineSteps', (itemsJson) => {
            console.log('loading items callback, reply:' + itemsJson);
            var items = [
            ];

            if(itemsJson && itemsJson[0] == '[') {
                console.log('Got items from storage, str looks like an array');
                try {
                    items = JSON.parse(itemsJson);  // Ensure jsonString is a well-formed JSON string
                } catch (e) {
                    console.error('Unable to parse items from storage! Error:');
                    console.error(e);
                    console.error('Storage value:');
                    console.error(itemsJson);
                    pluginDialogs.Alert("Unable to parse the layout value for Routine manager storage due to JSON error: `"+e.message+"`.\n\n"+
                        "To avoid data loss, please do NOT save this file and immediately copy any important data to a Notepad document.\n\n"+
                        "Please contact support for assistance.");
                    return;
                }
                console.log('Parsed items');
            } else {
                console.log('No items in storage');
            }

            console.log('items to display:', items);

            if(!testOnly) {
                // Render loaded items to page
                for (let item of items) {
                    console.log(item);
                    const $itemDiv = $('<div></div>').addClass('routineStep')
                                                     .addClass('ui-state-default')
                                                     //.css('left', item.x)
                                                     //.css('top', item.y)
                                                     //.css('width', item.width)
                                                     //.css('height', item.height)
                                                     .html(routineTemplate.replace('{doneFlag}', item.doneFlag ? 'done' : 'not-done')
                                                                          .replace('{title}', item.text));
                    $('#routineList').append($itemDiv);
                    bindItemEvents($itemDiv);
                }


                $('#routineList').sortable({
                    items: '.routineStep',
                    handle: '.draghandle',
                    stop: function(event, ui) {
                        relabelRoutines();
                    }
                });

                relabelRoutines();

            }
        });
    }

    function bindItemEvents($itemDiv) {
        /*
        // After item dragging:
        $itemDiv.on("dragstop", function(event, ui) {
            saveRoutines();
        });

        // After item resizing:
        let resizingItem = false;

        $itemDiv.on('mousedown touchstart', function() {
            resizingItem = true;
        });

        $(document).on('mouseup touchend', function() {
            if (resizingItem) {
                saveRoutines();
                resizingItem = false;
            }
        });
        */

        $itemDiv.find('.done-btn').on('click', function() {
            $(this).toggleClass('not-done');
            $(this).toggleClass('done');
            saveRoutines();
        })

        // Make item body editable on click:
        $itemDiv.on('click', function() {
            if (!$(this).find('.body').attr('contenteditable')) {
                $(this).find('.body').attr('contenteditable', 'true');
                $(this).find('.body').focus();
            }
        });

        // After item content modification:
        $itemDiv.on('blur', function() {
            $(this).find('.body').removeAttr('contenteditable');
            saveRoutines();
        });

        $itemDiv.find('.delete-item').on('click', () => {
            if(confirm('Are you sure you want to delete the routine step that starts with:\n\n'+$itemDiv.find('.body').text().substr(0, 200))) {
                $itemDiv.remove();
            }
        })
    }

    function saveRoutines() {
        const savedItems = $('.routineStep').map(function() {
            return {
                text: $(this).find('.body').html(),
                doneFlag: $(this).find('.done-btn').hasClass('done')
            };
        }).get();
        console.log('RoutineSteps to save: '+JSON.stringify(savedItems));
        pluginStorage.WriteLayoutValue('RoutineSteps', JSON.stringify(savedItems), function() {
            // Verify by reloading, should be no errors
            loadRoutines(true);
        });
        localStorage.setItem('RoutineSteps', JSON.stringify(savedItems));
    }

    function relabelRoutines() {
        var count = 0;
        $('.title').each(function(idx,item) {
            count++;
            console.log(count, item);
            item.innerHTML = 'Routine Step ' + count;
        })
    }



    loadRoutines();

    // Setup button handlers
    $('#addItem').on('click', () => {
        // Create a new item
        const $itemDiv = $('<div></div>').addClass('routineStep')
                                         .addClass('ui-state-default')
                                         //.css('left', 40)
                                         //.css('top', 40)
                                         //.css('width', 200)
                                         //.css('height', 100)
                                         .html(routineTemplate.replace('{doneFlag}', 'not-done').replace('{title}', 'New routine step'));
        $('#routineList').append($itemDiv);
        relabelRoutines();

        bindItemEvents($itemDiv);
    });

    $('#clearDoneFlags').on('click', () => {
        $('.done').toggleClass('not-done').toggleClass('done');
        saveRoutines();
    });


    window.onbeforeunload = function() {
        saveRoutines();
    }

});

