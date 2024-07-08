let keras_model;

let prediction_count = 0;
let boot_prediction_count = 0;
let gradient_count = 0;
let lineup_prediction_count = 0;
let lineup_boot_prediction_count = 0;
let lineup_gradient_count = 0;

async function get_weight(name) {
  let folder = "layer_weights/";
  const response = await fetch(folder.concat(name).concat(".json"));
  const json = await response.json();
  const array_of_tensor = json.map(element => tf.tensor(element));
  return array_of_tensor;
}

async function load_model() {
  
  // Input layer
  input_1 = tf.layers.input({
    shape: [32, 32, 1], 
    name: "input_1"});
  
  // Block 1
  block1_conv1 = tf.layers.conv2d({
    filters: 32, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block1_conv1", 
    weights: await get_weight("block1_conv1"),
    trainable: false});
  batch_normalization = tf.layers.batchNormalization({
    name: "batch_normalization", 
    weights: await get_weight("batch_normalization"), 
    trainable: false});
  activation = tf.layers.reLU({name: "activation"});
  
  block1_conv2 = tf.layers.conv2d({
    filters: 32, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block1_conv2", 
    weights: await get_weight("block1_conv2"),
    trainable: false});
  batch_normalization_1 = tf.layers.batchNormalization({
    name: "batch_normalization_1", 
    weights: await get_weight("batch_normalization_1"), 
    trainable: false});
  activation_1 = tf.layers.reLU({name: "activation_1"});
  
  block1_pool = tf.layers.maxPooling2d({
    poolSize: [2, 2], 
    strides: [2, 2], 
    name: "block1_pool"});
  
  // Block 2
  block2_conv1 = tf.layers.conv2d({
    filters: 64, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block2_conv1", 
    weights: await get_weight("block2_conv1"),
    trainable: false});
  batch_normalization_2 = tf.layers.batchNormalization({
    name: "batch_normalization_2", 
    weights: await get_weight("batch_normalization_2"), 
    trainable: false});
  activation_2 = tf.layers.reLU({name: "activation_2"});
  
  block2_conv2 = tf.layers.conv2d({
    filters: 64, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block2_conv2", 
    weights: await get_weight("block2_conv2"),
    trainable: false});
  batch_normalization_3 = tf.layers.batchNormalization({
    name: "batch_normalization_3", 
    weights: await get_weight("batch_normalization_3"), 
    trainable: false});
  activation_3 = tf.layers.reLU({name: "activation_3"});
  
  block2_pool = tf.layers.maxPooling2d({
    poolSize: [2, 2], 
    strides: [2, 2], 
    name: "block2_pool"});
    
  // Block 3
  block3_conv1 = tf.layers.conv2d({
    filters: 128, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block3_conv1", 
    weights: await get_weight("block3_conv1"),
    trainable: false});
  batch_normalization_4 = tf.layers.batchNormalization({
    name: "batch_normalization_4", 
    weights: await get_weight("batch_normalization_4"), 
    trainable: false});
  activation_4 = tf.layers.reLU({name: "activation_4"});
  
  block3_conv2 = tf.layers.conv2d({
    filters: 128, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block3_conv2", 
    weights: await get_weight("block3_conv2"),
    trainable: false});
  batch_normalization_5 = tf.layers.batchNormalization({
    name: "batch_normalization_5", 
    weights: await get_weight("batch_normalization_5"), 
    trainable: false});
  activation_5 = tf.layers.reLU({name: "activation_5"});
  
  block3_conv3 = tf.layers.conv2d({
    filters: 128, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block3_conv3", 
    weights: await get_weight("block3_conv3"),
    trainable: false});
  batch_normalization_6 = tf.layers.batchNormalization({
    name: "batch_normalization_6", 
    weights: await get_weight("batch_normalization_6"), 
    trainable: false});
  activation_6 = tf.layers.reLU({name: "activation_6"});
  
  block3_pool = tf.layers.maxPooling2d({
    poolSize: [2, 2], 
    strides: [2, 2], 
    name: "block3_pool"});
    
  // Block 4
  block4_conv1 = tf.layers.conv2d({
    filters: 256, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block4_conv1", 
    weights: await get_weight("block4_conv1"),
    trainable: false});
  batch_normalization_7 = tf.layers.batchNormalization({
    name: "batch_normalization_7", 
    weights: await get_weight("batch_normalization_7"), 
    trainable: false});
  activation_7 = tf.layers.reLU({name: "activation_7"});
  
  block4_conv2 = tf.layers.conv2d({
    filters: 256, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block4_conv2", 
    weights: await get_weight("block4_conv2"),
    trainable: false});
  batch_normalization_8 = tf.layers.batchNormalization({
    name: "batch_normalization_8", 
    weights: await get_weight("batch_normalization_8"), 
    trainable: false});
  activation_8 = tf.layers.reLU({name: "activation_8"});
  
  block4_conv3 = tf.layers.conv2d({
    filters: 256, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block4_conv3", 
    weights: await get_weight("block4_conv3"),
    trainable: false});
  batch_normalization_9 = tf.layers.batchNormalization({
    name: "batch_normalization_9", 
    weights: await get_weight("batch_normalization_9"), 
    trainable: false});
  activation_9 = tf.layers.reLU({name: "activation_9"});
  
  block4_pool = tf.layers.maxPooling2d({
    poolSize: [2, 2], 
    strides: [2, 2], 
    name: "block4_pool"});
    
  // Block 5
  block5_conv1 = tf.layers.conv2d({
    filters: 256, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block5_conv1", 
    weights: await get_weight("block5_conv1"),
    trainable: false});
  batch_normalization_10 = tf.layers.batchNormalization({
    name: "batch_normalization_10", 
    weights: await get_weight("batch_normalization_10"), 
    trainable: false});
  activation_10 = tf.layers.reLU({name: "activation_10"});
  
  block5_conv2 = tf.layers.conv2d({
    filters: 256, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block5_conv2", 
    weights: await get_weight("block5_conv2"),
    trainable: false});
  batch_normalization_11 = tf.layers.batchNormalization({
    name: "batch_normalization_11", 
    weights: await get_weight("batch_normalization_11"), 
    trainable: false});
  activation_11 = tf.layers.reLU({name: "activation_11"});
  
  block5_conv3 = tf.layers.conv2d({
    filters: 256, 
    kernelSize: [3, 3], 
    padding: "same", 
    name: "block5_conv3", 
    weights: await get_weight("block5_conv3"),
    trainable: false});
  batch_normalization_12 = tf.layers.batchNormalization({
    name: "batch_normalization_12", 
    weights: await get_weight("batch_normalization_12"), 
    trainable: false});
  activation_12 = tf.layers.reLU({name: "activation_12"});
  
  block5_pool = tf.layers.maxPooling2d({
    poolSize: [2, 2], 
    strides: [2, 2], 
    name: "block5_pool"});

  // Global pooling
  global_max_pooling2d = tf.layers.globalMaxPooling2d({name: "global_max_pooling2d"});
  
  // Concatenate
  additional_input = tf.layers.input({shape: [5], name: "additional_input"});
  concatenate = tf.layers.concatenate();
  
  // Prediction block
  dense = tf.layers.dense({
    units: 256, 
    name: "dense", 
    weights: await get_weight("dense"), 
    trainable: false});
  activation_13 = tf.layers.reLU({name: "activation_13"});
  dense_1 = tf.layers.dense({
    units: 1,
    activation: "relu",
    name: "dense_1", 
    weights: await get_weight("dense_1"), 
    trainable: false});
  
  // Get output
  output = block1_conv1.apply(input_1);
  output = batch_normalization.apply(output);
  output = activation.apply(output);
  output = block1_conv2.apply(output);
  output = batch_normalization_1.apply(output);
  output = activation_1.apply(output);
  output = block1_pool.apply(output);
  
  output = block2_conv1.apply(output);
  output = batch_normalization_2.apply(output);
  output = activation_2.apply(output);
  output = block2_conv2.apply(output);
  output = batch_normalization_3.apply(output);
  output = activation_3.apply(output);
  output = block2_pool.apply(output);
  
  output = block3_conv1.apply(output);
  output = batch_normalization_4.apply(output);
  output = activation_4.apply(output);
  output = block3_conv2.apply(output);
  output = batch_normalization_5.apply(output);
  output = activation_5.apply(output);
  output = block3_conv3.apply(output);
  output = batch_normalization_6.apply(output);
  output = activation_6.apply(output);
  output = block3_pool.apply(output);
  
  output = block4_conv1.apply(output);
  output = batch_normalization_7.apply(output);
  output = activation_7.apply(output);
  output = block4_conv2.apply(output);
  output = batch_normalization_8.apply(output);
  output = activation_8.apply(output);
  output = block4_conv3.apply(output);
  output = batch_normalization_9.apply(output);
  output = activation_9.apply(output);
  output = block4_pool.apply(output);
  
  output = block5_conv1.apply(output);
  output = batch_normalization_10.apply(output);
  output = activation_10.apply(output);
  output = block5_conv2.apply(output);
  output = batch_normalization_11.apply(output);
  output = activation_11.apply(output);
  output = block5_conv3.apply(output);
  output = batch_normalization_12.apply(output);
  output = activation_12.apply(output);
  output = block5_pool.apply(output);
  
  output = global_max_pooling2d.apply(output);
  output = concatenate.apply([output, additional_input]);
  output = dense.apply(output);
  output = activation_13.apply(output);
  output = dense_1.apply(output);
  
  // Construct model
  this_model = tf.model({inputs: [input_1, additional_input], outputs: output});
    
  this_model.compile({
    optimizer: 'adam',
    loss: 'meanSquaredError',
    metrics: ['accuracy']});
    
  keras_model = this_model;
  Shiny.setInputValue("model_loaded", 1);
}

async function predict() {
  
  // vgg16 input preprocessing
  const input_tensor = tf.image.rgbToGrayscale(
    tf.sub(
      tf.tensor(array_input_data), 
      tf.tensor([[[[103.939, 116.779, 123.68]]]])
      )
    );
  const auxiliary_tensor = tf.tensor(array_auxiliary_data);
  
  const value = keras_model.predict([input_tensor, auxiliary_tensor]).arraySync();
  prediction_count = prediction_count + 1;
  Shiny.setInputValue("prediction", [prediction_count, value]);
}

async function predict_lineup() {
  
  // vgg16 input preprocessing
  const input_tensor = tf.image.rgbToGrayscale(
    tf.sub(
      tf.tensor(array_lineup_input_data), 
      tf.tensor([[[[103.939, 116.779, 123.68]]]])
      )
    );
  const auxiliary_tensor = tf.tensor(array_lineup_auxiliary_data);
  
  const value = keras_model.predict([input_tensor, auxiliary_tensor]).arraySync();
  lineup_prediction_count = lineup_prediction_count + 1;
  Shiny.setInputValue("lineup_prediction", [lineup_prediction_count, value]);
}

async function predict_boot() {
  // vgg16 input preprocessing
  const input_tensor = tf.image.rgbToGrayscale(
    tf.sub(
      tf.tensor(array_boot_input_data), 
      tf.tensor([[[[103.939, 116.779, 123.68]]]])
      )
    );
  const auxiliary_tensor = tf.tensor(array_boot_auxiliary_data);
  
  const value = keras_model.predict([input_tensor, auxiliary_tensor]).arraySync();
  boot_prediction_count = boot_prediction_count + 1;
  Shiny.setInputValue("boot_prediction", [boot_prediction_count, value]);
}

async function predict_lineup_boot() {
  
  // vgg16 input preprocessing
  const input_tensor = tf.image.rgbToGrayscale(
    tf.sub(
      tf.tensor(array_lineup_boot_input_data), 
      tf.tensor([[[[103.939, 116.779, 123.68]]]])
      )
    );
  const auxiliary_tensor = tf.tensor(array_lineup_boot_auxiliary_data);
  
  const value = keras_model.predict([input_tensor, auxiliary_tensor]).arraySync();
  lineup_boot_prediction_count = lineup_boot_prediction_count + 1;
  Shiny.setInputValue("lineup_boot_prediction", [lineup_boot_prediction_count, value]);
}

async function get_grad() {
  
  // vgg16 input preprocessing
  const input_tensor = tf.image.rgbToGrayscale(
    tf.sub(
      tf.tensor(array_input_data), 
      tf.tensor([[[[103.939, 116.779, 123.68]]]])
      )
    );
  const auxiliary_tensor = tf.tensor(array_auxiliary_data);
  
  const f = x => keras_model.predict([x, auxiliary_tensor]);
  
  const g = tf.grad(f);
  const value = g(input_tensor).arraySync();
  gradient_count = gradient_count + 1;
  Shiny.setInputValue("gradient", [value, gradient_count]);
}

async function get_lineup_grad() {
  
  // vgg16 input preprocessing
  const input_tensor = tf.image.rgbToGrayscale(
    tf.sub(
      tf.tensor(array_lineup_input_data), 
      tf.tensor([[[[103.939, 116.779, 123.68]]]])
      )
    );
  const auxiliary_tensor = tf.tensor(array_lineup_auxiliary_data);
  
  const f = x => keras_model.predict([x, auxiliary_tensor]);
  
  const g = tf.grad(f);
  const value = g(input_tensor).arraySync();
  lineup_gradient_count = lineup_gradient_count + 1;
  Shiny.setInputValue("lineup_gradient", [value, lineup_gradient_count]);
}

load_model();
