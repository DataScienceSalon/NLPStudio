#'  @param x The principal object
#'  @param key Character string or a vector of strings containing a key or
#'  keys for metadata key/value pair(s).
#'  @param value Character or numeric, or a vector thereof, containing a value
#'  or values for metadata key/value pair(s).
#'  @param format Character string used in the metadata method. Indicates
#'  whether to return the metadata as  list of lists, or a data.frame.This method
#'  returns metadata in a list if no key/value pair is provided. The list
#'  may contain other lists or data frames associated with the type of
#'  metadata. This parameter specifies whether to return the metadata
#'  as a list of lists, or a list of data frames. Valid values are:
#'  c('list', 'data.frame')
