'use strict';

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