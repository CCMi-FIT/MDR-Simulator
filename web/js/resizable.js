$("#ufob-box").resizable({
  handles: 'e',
  resize: function() {
    $("#diag-ufoa-inst").outerWidth($("#ufoa-inst-tab").innerWidth() - $("#ufob-box").outerWidth());
  }
});
