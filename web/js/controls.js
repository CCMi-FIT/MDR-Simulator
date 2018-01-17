'use strict';

var currentTab = '#simulation-tab';

$('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
  var target = $(e.target).attr("href") // activated tab
  if (target == '#simulation-tab') {
    $('#controls').css('display', 'inline-block');
    $('#help-btn').css('display', 'inline-block');
  } else {
    $('#controls').css('display', 'none');
    $('#help-btn').css('display', 'none');
  }
  currentTab = target;
});

function gotoModel() {
  $('#model-page').css('display', 'block');
  $('#methodology-page').css('display', 'none');
  $('#about-page').css('display', 'none');
  $('#model-mi').addClass("active");
  $('#methodology-mi').removeClass("active");
  $('#about-mi').removeClass("active");
}

function gotoMethodology() {
  $('#model-page').css('display', 'none');
  $('#methodology-page').css('display', 'block');
  $('#about-page').css('display', 'none');
  $('#model-mi').removeClass("active");
  $('#methodology-mi').addClass("active");
  $('#about-mi').removeClass("active");
}

function gotoAbout() {
  $('#model-page').css('display', 'none');
  $('#methodology-page').css('display', 'none');
  $('#about-page').css('display', 'block');
  $('#model-mi').removeClass("active");
  $('#methodology-mi').removeClass("active");
  $('#about-mi').addClass("active");
}

function gotoEvaluation() {
  $('.nav-tabs a[href="#domain-tab"]').tab('show');
}

function downloadDiagram() {
  var d;
  switch (currentTab) {
    case '#simulation-tab': d = 'ufob.svg'; break;
    case '#ufoa-tab': d = 'ufoa.svg'; break;
    case '#domain-tab': d = 'domain.png'; break;
  }
  window.open('img/' + d);
}