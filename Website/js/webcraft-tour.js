/*
Guided Tour Framework
by Nikolay Dyankov
November 27, 2015

- Uses Local Storage to track progress
- Depends on Bootstrap
- Occupies the "WebcraftGuidedTour" global namespace
- App responsibilities:
    - Define steps (text, position - left/right/top/bottom)
    - Manually trigger next step
    - Provide callbacks for the absolute position of the windows
- Framework responsibilities:
    - Keep track of the current step
    - Create step windows
    - Show/hide step windows
*/

function WebcraftGuidedTour(container) {
    this.steps = new Array();
    this.container = container;
    this.ended = false;
}

;(function ($, window, document, undefined ) {
    WebcraftGuidedTour.prototype.init = function(container) {
        // Check for Local Storage support
        if (!this.localStorageSupport()) return;

        // Should start tour?
        if (localStorage.webcraftGuidedTour && localStorage.webcraftGuidedTour == this.steps.length) {
            this.ended = true;
            return;
        }

        // Start
        if (!localStorage.webcraftGuidedTour) localStorage.webcraftGuidedTour = 0;
        this.presentWindow(localStorage.webcraftGuidedTour);

        // Events
        var self = this;
        $(document).on('click', '#webcraft-tour-button-close', function() {
            self.end();
        });
        $(document).on('click', '#webcraft-tour-button-next', function() {
            self.next();
        });
        $(document).on('click', '#webcraft-tour-button-back', function() {
            self.prev();
        });
    };
    WebcraftGuidedTour.prototype.addStep = function(name, orientation, title, content, coordinatesCallback) {
        this.steps.push({
            name: name,
            orientation: orientation,
            title: title,
            content: content,
            coordinatesCallback: coordinatesCallback
        });

        var self = this;
        return function() {
            self.completeStep(name);
        }
    };
    WebcraftGuidedTour.prototype.completeStep = function(name) {
        // Find out the step index
        var index = 0;
        for (var i=0; i<this.steps.length; i++) {
            if (this.steps[i].name == name) {
                index = i;
                break;
            }
        }

        // If the step is currently active, proceed to the next step
        if (index == localStorage.webcraftGuidedTour) {
            this.next();
        }
    };
    WebcraftGuidedTour.prototype.next = function() {
        if (localStorage.webcraftGuidedTour < this.steps.length - 1) {
            localStorage.webcraftGuidedTour++;
            this.presentWindow(localStorage.webcraftGuidedTour);
        } else {
            this.end();
        }
    };
    WebcraftGuidedTour.prototype.prev = function() {
        if (localStorage.webcraftGuidedTour > 0) {
            localStorage.webcraftGuidedTour--;
            this.presentWindow(localStorage.webcraftGuidedTour);
        }
    };
    WebcraftGuidedTour.prototype.end = function() {
        localStorage.webcraftGuidedTour = this.steps.length;
        $('#webcraft-tour-popover').remove();
        this.ended = true;
    };
    WebcraftGuidedTour.prototype.resumeFromStep = function(name) {
        // If the tour ended, return
        if (localStorage.webcraftGuidedTour == this.steps.length) return;

        // Find out the step index
        var index = 0;
        for (var i=0; i<this.steps.length; i++) {
            if (this.steps[i].name == name) {
                index = i;
                break;
            }
        }

        localStorage.webcraftGuidedTour = index;
        this.presentWindow(index);
    };
    WebcraftGuidedTour.prototype.presentWindow = function(i) {
        // Hide current window
        $('#webcraft-tour-popover').remove();

        var backButtonClass = 'pull-right';
        var closeButtonClass = 'pull-left btn-default';

        if (localStorage.webcraftGuidedTour == this.steps.length - 1) {
            backButtonClass = 'pull-left';
            closeButtonClass = 'pull-right btn-primary';
        }

        // Generate the window HTML
        var html = '';

        html += '   <div id="webcraft-tour-popover" class="popover '+ this.steps[i].orientation +' fade">';
        html += '       <div class="arrow"></div>';
        html += '       <h3 class="popover-title">'+ this.steps[i].title +'<div id="webcraft-tour-steps-counter">'+ localStorage.webcraftGuidedTour +'/'+ (this.steps.length - 1) +'</div></h3>';
        html += '       <div class="popover-content">'+ this.steps[i].content;
        html += '           <div id="webcraft-tour-button-close" class="btn '+ closeButtonClass +'">Close</div>';

        html += '           <div class="btn-group '+ backButtonClass +'">';
        html += '               <div id="webcraft-tour-button-back" class="btn btn-default">Back</div>';

        if (i < this.steps.length - 1) {
            html += '           <div id="webcraft-tour-button-next" class="btn btn-primary">Next</div>';
        }

        html += '           </div>';

        html += '       </div>';
        html += '   </div>';

        // Display
        this.container.append(html);

        // Position
        var p = this.steps[i].coordinatesCallback();
        var popover = $('#webcraft-tour-popover');

        if (this.steps[i].orientation == 'left') {
            popover.css({
                left: p.x - popover.outerWidth(),
                top: p.y - popover.outerHeight()/2
            });
        }
        if (this.steps[i].orientation == 'right') {
            popover.css({
                left: p.x,
                top: p.y - popover.outerHeight()/2
            });
        }
        if (this.steps[i].orientation == 'top') {
            popover.css({
                left: p.x - popover.outerWidth()/2,
                top: p.y - popover.outerHeight()
            });
        }
        if (this.steps[i].orientation == 'bottom') {
            popover.css({
                left: p.x - popover.outerWidth()/2,
                top: p.y
            });
        }
        if (this.steps[i].orientation == '') {
            popover.css({
                left: p.x - popover.outerWidth()/2,
                top: p.y - popover.outerHeight()/2
            });
        }

        // Present
        setTimeout(function() {
            $('#webcraft-tour-popover').css({ opacity: 1 });
        }, 1);
    };
    WebcraftGuidedTour.prototype.localStorageSupport = function() {
        // Support check
        try {
            var storage = window['localStorage'],
            x = '__storage_test__';
            storage.setItem(x, x);
            storage.removeItem(x);
            return true;
        }
        catch(e) {
            console.log('Local storage is NOT supported!');
            return false;
        }
    };
})(jQuery, window, document);
