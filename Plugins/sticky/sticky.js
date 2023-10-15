$(document).ready(function(){
    const notes = [
        {x: 100, y: 100, width: 150, height: 150, text: "Note 1"},
        {x: 300, y: 200, width: 200, height: 100, text: "Note 2"},
        // Add more notes as needed
    ];

    for (let note of notes) {
        const $noteDiv = $('<div></div>').addClass('sticky-note')
                                         .css('left', note.x + 'px')
                                         .css('top', note.y + 'px')
                                         .css('width', note.width + 'px')
                                         .css('height', note.height + 'px')
                                         .text(note.text);
        $('body').append($noteDiv);
    }
});
