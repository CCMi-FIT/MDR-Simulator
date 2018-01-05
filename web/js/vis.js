$(function () {

  var nodes = new vis.DataSet([
      {id: '1', label: '<emph>Node 1</emph>'},
      {id: '2', label: 'Node 2'},
      {id: '3', label: 'Node 3'},
      {id: '4', label: 'Node 4'},
      {id: '5', label: 'Node 5'}
  ]);

  // create an array with edges
  var edges = new vis.DataSet([
      {from: '1', to: '3'},
      {from: '1', to: '2'},
      {from: '2', to: '4'},
      {from: '2', to: '5'}
  ]);

  // provide the data in the vis format
  var data = {
      nodes: nodes,
      edges: edges
  };
  var options = {};

  // create a network
  var container = document.getElementById('diag-ufoa-inst');
  var network = new vis.Network(container, data, options);
})
