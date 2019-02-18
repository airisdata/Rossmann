library(feather)
nn_tr <- feather::read_feather("/mnt/disks/data_disk/rossmann_data/neural_net_predictions/train_preds_df_func_full_model.feather")
nn_val <- feather::read_feather("/mnt/disks/data_disk/rossmann_data/neural_net_predictions/valid_preds_df_func_full_model.feather")
nn_tr$data_type <- "tr"
nn_val$data_type <- "val"

nn_full <- bind_rows(nn_tr, nn_val)
nn_full
