const pluginStorage = PluginStorageService();
const pluginDialogs = PluginDialogsService();

$(document).ready(function(){
    
    // Load notes from localStorage or a default set
    function loadCorkboard(testOnly=false) {
        pluginStorage.ReadLayoutValue('CorkNotes', (notesJson) => {
            console.log('loading notes callback, reply:' + notesJson);
            var notes = [
                { x: '10px', y: '40px', width: '150px', height: '80px', text: "This is the corkboard sticky notes plugin!"},
                { x: '30px', y: '140px', width: '200px', height: '160px', text: "Notes are currently stored in local storage, not the StoneNotes layout file or database.<br><b>This means that they will be lost when StoneNotes is shutdown.</b><br>This is just a test plugin for now!"},
            ];

            if(notesJson && notesJson[0] == '[') {
                console.log('Got notes from storage, str looks like an array');
                try {
                    notes = JSON.parse(notesJson);  // Ensure jsonString is a well-formed JSON string
                } catch (e) {
                    console.error('Unable to parse notes from storage! Error:');
                    console.error(e);
                    console.error('Storage value:');
                    console.error(notesJson);
                    pluginDialogs.Alert("Unable to parse the layout value for Sitcky Notes storage due to JSON error: `"+e.message+"`.\n\n"+
                        "To avoid data loss, please do NOT save this file and immediately copy any important data to a Notepad document.\n\n"+
                        "Please contact support for assistance.");
                    return;
                }
                console.log('Parsed notes');
            } else {
                console.log('No notes in storage');
            }

            console.log('notes to display:', notes);

            if(!testOnly) {
                // Render loaded notes to page
                for (let note of notes) {
                    console.log(note);
                    const $noteDiv = $('<div></div>').addClass('sticky-note')
                                                     .css('left', note.x)
                                                     .css('top', note.y)
                                                     .css('width', note.width)
                                                     .css('height', note.height)
                                                     .html('<div class="draghandle"><button class="delete-note">X</button></div><div class="body">'+note.text+'</div>');
                    $('body').append($noteDiv);
                    $noteDiv.draggable({handle: '.draghandle'});
                    bindNoteEvents($noteDiv);
                }
            }
        });
    }

    function bindNoteEvents($noteDiv) {
        // After note dragging:
        $noteDiv.on("dragstop", function(event, ui) {
            saveNotes();
        });

        // After note resizing:
        let resizingNote = false;

        $noteDiv.on('mousedown touchstart', function() {
            resizingNote = true;
        });

        $(document).on('mouseup touchend', function() {
            if (resizingNote) {
                saveNotes();
                resizingNote = false;
            }
        });

        // Make note body editable on click:
        $noteDiv.on('click', function() {
            if (!$(this).find('.body').attr('contenteditable')) {
                $(this).find('.body').attr('contenteditable', 'true');
                $(this).find('.body').focus();
            }
        });

        // After note content modification:
        $noteDiv.on('blur', function() {
            $(this).find('.body').removeAttr('contenteditable');
            saveNotes();
        });

        $noteDiv.find('.delete-note').on('click', () => {
            if(confirm('Are you sure you want to delete the note that starts with:\n\n'+$noteDiv.find('.body').text().substr(0, 200))) {
                $noteDiv.remove();
            }
        })
    }

    function saveNotes() {
        const savedNotes = $('.sticky-note').map(function() {
            return {
                x: $(this).css('left'),
                y: $(this).css('top'),
                width: $(this).css('width'),
                height: $(this).css('height'),
                text: $(this).find('.body').html()
            };
        }).get();
        console.log('CorkNotes to save: '+JSON.stringify(savedNotes));
        pluginStorage.WriteLayoutValue('CorkNotes', JSON.stringify(savedNotes));
        localStorage.setItem('CorkNotes', JSON.stringify(savedNotes));

        // Verify by reloading, should be no errors
        loadCorkboard(true);
    }


    function SeededRandom(seed) {
        this._seed = seed % 2147483647;
        if (this._seed <= 0) this._seed += 2147483646;
    }

    SeededRandom.prototype.next = function() {
        return this._seed = this._seed * 16807 % 2147483647;
    };

    SeededRandom.prototype.nextFloat = function() {
        return (this.next() - 1) / 2147483646;
    };

    function drawCorkboard() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
        
        const numberOfPolygons = 10000;
        const maxPolygonSize = 5;

         const colors = ['#c2b280', '#e0c09f', '#d3b791', '#bfa974', '#c9ad86', '#d0be9c', '#dbc1ae'];

        ctx.fillStyle = colors[4];
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        const rng = new SeededRandom(123456);  // Seed for deterministic randomness

        for (let i = 0; i < numberOfPolygons; i++) {
            const x = rng.nextFloat() * canvas.width;
            const y = rng.nextFloat() * canvas.height;
            const size = rng.nextFloat() * maxPolygonSize;
            const sides = Math.floor(rng.nextFloat() * 5) + 3; // Random number of sides between 3 (triangles) and 7

            ctx.beginPath();
            ctx.moveTo(x + size * Math.cos(0), y + size * Math.sin(0));

            for (let j = 1; j <= sides; j++) {
                ctx.lineTo(x + size * Math.cos(j * 2 * Math.PI / sides), y + size * Math.sin(j * 2 * Math.PI / sides));
            }
            
            ctx.closePath();
            ctx.fillStyle = colors[Math.floor(rng.nextFloat() * colors.length)];
            ctx.fill();
        }
    }

    loadCorkboard();

    // Setup background rendering of a corkboard effect
    const canvas = document.getElementById('corkboardCanvas');
    const ctx = canvas.getContext('2d');

    

    window.addEventListener('resize', drawCorkboard);
    drawCorkboard();

    // Setup button handlers
    $('#addNote').on('click', () => {
        // Create a new note
        const $noteDiv = $('<div></div>').addClass('sticky-note')
                                         .css('left', 40)
                                         .css('top', 40)
                                         .css('width', 200)
                                         .css('height', 100)
                                         .html('<div class="draghandle"><button class="delete-note">X</button></div><div class="body">New note!</div>');
        $('body').append($noteDiv);
        $noteDiv.draggable({handle: '.draghandle'});

        bindNoteEvents($noteDiv);
    });

    $('#saveNotes').on('click', () => {
        saveNotes();
    });

    window.onbeforeunload = function() {
        saveNotes();
    }

});
