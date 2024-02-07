function showname () {
  var photosNames = document.getElementById('carpetImagenes'); 
  
  const photosCollection = [];
  
  for (var i = 0; i < photosNames.files.length; i++) {
     photosCollection.push(photosNames.files.item(i).name)
  }     
  
  Shiny.setInputValue('photosNames', photosCollection);
};

