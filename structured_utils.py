def get_time_based_split(df,train_fraction=0.9,include_valid=False, valid_fraction=None,date_col="Date"):
    """
    Utility function to split a dataframe into train (validation) and test datasets based on a date column
    
    :param df: The dataframe to be split
    :type df: pandas.DataFrame
    :param train_fraction: fraction of data in train set (default:0.9)
    :type train_fraction: float
    :param include_valid: Whether to include a validation set (default: False)
    :type include_valid: bool
    :param valid_fraction: fraction of data in validation set if include_valid is True (default: None)
    :type valid_fraction: float
    :param date_col: Name of the date column on which to split the dataframe
    :type date_col: string
    
    :returns: (train_df,test_df) or (train_df,valid_df,test_df) if include_valid is True
    """
    import pandas as pd
    tmp_df=df
    tmp_df[date_col]= pd.to_datetime(tmp_df[date_col])
    tmp_df.sort_values(by=date_col,inplace=True,ascending=True)
    train_dim=int(train_fraction*len(df))
    train_df= tmp_df.iloc[:train_dim]
    if include_valid:
        valid_dim= int((train_fraction+valid_fraction)*len(df))
        valid_df= tmp_df.iloc[train_dim:valid_dim]
        test_df=tmp_df.iloc[valid_dim:]
        return train_df,valid_df,test_df
    else:
        test_df=tmp_df.iloc[train_dim:]
        return train_df,test_df
    
def get_embedding_dim(df, cat_vars,min_dim=1,max_dim=50):
    """
    Utility function to get the embedding dimensions and cardinality of categorical columns
    
    :param df: The dataframe to be processed
    :type df: pandas.DataFrame
    :param cat_vars: List of names of categorical columns
    :type cat_vars: list(string)
    :param min_dim: Minimum Embedding Dimension (default:1) 
    :type min_dim: int
    :param max_dim: Maximum Embedding Dimension (default:50) 
    :type max_dim: int
    
    :returns:Dictionary of column_name:(embedding_dimension,cardinality) for all categorical columns
    :type: dict
    """
    def get_cardinality(df,col):
        return df[col].nunique()
    def emb_dim(cardinality,min_dim,max_dim):
        return min(max((cardinality+1)//2,min_dim),max_dim)
    
    cat_dict= {i: (emb_dim(get_cardinality(df,i),min_dim,max_dim),get_cardinality(df,i)) for i in sorted(cat_vars)} 
    return cat_dict

def impute_cols(df,cat_vars,contin_vars,cat_val="No Value",contin_val=0.0):
    """
    Seperately impute Categorical and Continuous variables of a Dataframe
    
    :param df: The dataframe to be processed
    :type df: pandas.DataFrame
    :param cat_vars: List of names of continuous columns
    :type cat_vars: list(string)
    :param contin_vars: List of names of categorical columns
    :type contin_vars: list(string)
    :param cat_val: Value to fill in for categorical columns (default: "No value")
    :type cat_val: string
    :param contin_val: Value to fill in for categorical columns (default: 0.0)
    :type contin_val: float
    
    :returns:  DataFrame with null values filled
    :type: pandas.DataFrame
    
    """
    
    tmp_df=df
    tmp_df.fillna({a:cat_val for a in cat_vars},inplace=True)
    tmp_df.fillna({a:contin_val for a in contin_vars},inplace=True)
    return tmp_df

def fit_transformers(df,cat_vars,contin_vars):
    """
    Create and Fit the dataframe mapper objects for the continuous and Categorical columns
    
    :param df: The dataframe to be processed
    :type df: pandas.DataFrame
    :param cat_vars: List of names of continuous columns
    :type cat_vars: list(string)
    :param contin_vars: List of names of categorical columns
    :type contin_vars: list(string)
    
    :returns:  (Categorical Dataframe Mapper, Continuous Dataframe Mapper)
    :type: (sklearn_pandas.DataFrameMapper,sklearn_pandas.DataFrameMapper)
    
    """
    from sklearn.preprocessing import LabelEncoder, StandardScaler
    from sklearn_pandas import DataFrameMapper
    import pandas as pd
    cat_maps = [(o,  LabelEncoder()) for o in sorted(cat_vars)]
    contin_maps = [([o], StandardScaler()) for o in sorted(contin_vars)]
    cat_mapper = DataFrameMapper(cat_maps)
    contin_mapper = DataFrameMapper(contin_maps)
    cat_map_fit = cat_mapper.fit(df)
    contin_map_fit = contin_mapper.fit(df)
    return  cat_map_fit, contin_map_fit

def transform_dataset(df,cat_mapper,contin_mapper):
    """
    Transform a dataframe with the categorical and continuous dataframe mappers
    
    :param df: The dataframe to be processed
    :type df: pandas.DataFrame
    :param cat_mapper: DataFrame Mapper for categorical Columns
    :type cat_mapper: sklearn_pandas.DataFrameMapper
    :param contin_mapper: DataFrame Mapper for continuous Columns
    :type contin_mapper: sklearn_pandas.DataFrameMapper
    
    :returns: the transformed final training object. List of [categorical_column1, categorical_column2..categorical_columnN, continuous_columns]
    :type:list(nump.Ndarray)
    """
    
    import numpy as np
    def split_cols(arr): 
        return np.hsplit(arr,arr.shape[1])
    cat_map_df=cat_mapper.transform(df).astype(np.int64)
    contin_map_df=contin_mapper.transform(df).astype(np.float32) 
    return split_cols(cat_map_df) + [contin_map_df]

def get_embedding_submodel(feat,cat_var_dict):
    """
    Get the embedding submodel for one caterical column
    
    :param feat: name of the categorical feature
    :type feat: string
    :param cat_var_dict: The dictionary created by get_embedding_dim function (name:(embedding_dimesion,cardinality))
    :type: dict
    
    :returns: The submodel processing the categorical column
    :type: tensorflow.keras.models.Model
    """
    
    from tensorflow.keras.layers import Input, Embedding
    from tensorflow.keras.models import Model
    
    name, (embedding_dim,cardinality) = feat,cat_var_dict[feat]
    inp = Input((1,), dtype='int64', name=name+'_input')
    emb=Embedding(cardinality,embedding_dim, input_length=1,\
                  embeddings_initializer="uniform",name=name+"_embed")(inp)
    model= Model(inp,emb,name=name+"_submodel")
    return model