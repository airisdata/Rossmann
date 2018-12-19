def get_train_and_valid_results(model,train_input,valid_input,train_df,valid_df,model_weights_path=None,return_col_list=None,prediction_col_name="Prediction"):
    if model_weights_path is not None:
        model.load_weights(f'{model_weights_path}')
    pred_train=model.predict(train_input)
    pred_valid=model.predict(valid_input)
    if return_col_list is not None:    
        save_train= train_df[return_col_list]
        save_valid= valid_df[return_col_list]
    else:
        save_train= train_df
        save_valid= valid_df
    save_train[prediction_col_name]=pred_train
    save_valid[prediction_col_name]=pred_valid
    return save_train,save_valid

def plot_history(hist):
    import matplotlib.pyplot as plt
    plt.figure(figsize=(20,8))
    plt.suptitle("Training Curve",fontsize=20)
    plt.plot(hist.history["loss"],'ro-',label="Training loss")
    plt.plot(hist.history["val_loss"],'bo-',label="Validation loss")
    plt.grid()
    plt.xlabel("Epoch",fontsize=18)
    plt.ylabel("Loss",fontsize=18)
    plt.legend(fontsize=18)
    
def generate_preds_df(model,joined_load_loc,cat_mapper,contin_mapper,output_col,model_weights_path=None):
    import feather
    from structured_utils import impute_cols,transform_dataset
    import pandas as pd
    
    joined_test=feather.read_dataframe(joined_load_loc)
    cat_var_list=[i[0] for i in cat_mapper.features]
    contin_var_list=[i[0][0] for i in contin_mapper.features]
    test_df= impute_cols(joined_test,cat_var_list,contin_var_list)
    map_test= transform_dataset(test_df,cat_mapper,contin_mapper) 
    if model_weights_path is not None:
        model.load_weights(f"{model_weights_path}")
    
    test_preds=model.predict(map_test)
    preds_submit= pd.DataFrame(joined_test["Id"])
    preds_submit[output_col]=test_preds
    return preds_submit    

def plot_results(filter_field,filter_value,train_df,valid_df,label_col,prediction_col):
    train_df[train_df[filter_field]==filter_value].plot(y=[label_col,prediction_col],figsize=(25,10),grid=True)
    valid_df[valid_df[filter_field]==filter_value].plot(y=[label_col,prediction_col],figsize=(25,10),grid=True)
    return

def create_tboard_logs(model,cat_mapper,model_weights_path=None,tboard_logdir="./tboard_logs"):
    import tensorflow as tf
    import tensorflow.keras as keras
    import numpy as np
    if model_weights_path is not None:
        model.load_weights(model_weights_path)
    saver = tf.train.Saver()
    sess = keras.backend.get_session()
    save_path = saver.save(sess, f"{tboard_logdir}/model.ckpt")
    
    total_string=""
    for i in cat_mapper.features:
        np.savetxt(f"tboard_logs/{i[0]}_metadata.tsv",i[1].classes_,fmt='%s', delimiter="\n")
        write_string=f'''
                     embeddings {{
                                 tensor_name: "{i[0]}_embed/embeddings"
                                 metadata_path:"{i[0]}_metadata.tsv" 
                    }}
                    '''
        total_string= total_string+write_string
        
    with open(f"{tboard_logdir}/projector_config.pbtxt","w") as f:
        print(total_string,file=f)   
    return

def model_fitter(model,train_x,train_y,validation_data,\
                 checkpoint_path,previous_weights_path=None,checkpoint_verbosity=0,\
                 max_epochs=100,batch_size=512):
    
    from tensorflow.keras.callbacks import LearningRateScheduler,ModelCheckpoint,TerminateOnNaN,EarlyStopping,ReduceLROnPlateau
    from keras_tqdm import TQDMNotebookCallback
    
    checkpoint= ModelCheckpoint(checkpoint_path,monitor="val_loss",save_best_only=True,verbose=checkpoint_verbosity)
    term= TerminateOnNaN()
    tqdm_callback= TQDMNotebookCallback(leave_inner=True,metric_format='{name}: {value:0.5f}')
    early_stop= EarlyStopping(monitor="val_loss",patience=8,min_delta=0,mode="min",verbose=1)
    reduce_lr=ReduceLROnPlateau(monitor='val_loss', factor=0.2, patience=4, verbose=1, mode='auto', min_delta=0.0005, cooldown=0, min_lr=1e-6)
    try:
        model.load_weights(previous_weights_path)
        print("Loading model Weights from previous model")
    except:
        print("Not loading model Weights from previous model")
    hist = model.fit(train_x,train_y, batch_size=batch_size, epochs=max_epochs,
                     verbose=0, validation_data=validation_data,callbacks=[checkpoint,reduce_lr,term,tqdm_callback,early_stop])

    plot_history(hist)

    print("Reloading Best Checkpointed weights")
    model.load_weights(checkpoint_path)
    print("Model Training Loss (Without Dropout)=",model.evaluate(train_x,train_y,verbose=0))
    print("Model Validation Loss=",model.evaluate(validation_data[0],validation_data[1],verbose=0))
    return hist,model

def visualize_model(model):
    from IPython.display import SVG
    from keras.utils.vis_utils import model_to_dot

    return SVG(model_to_dot(model,rankdir="LR",show_layer_names=True).create(prog='dot', format='svg'))