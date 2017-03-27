-type name()   :: string().
-type value()  :: string() | binary().
-type xstype() :: 'function item' | 'node()' | 'text()' | 
                  'empty-sequence()' | 'processing-instruction()' | 
                  'element()' | 'document-node()' | 'document-node(element())' | 
                  'attribute()' | 'comment()' | 'item()' | 'xs:untyped' | 
                  'xs:anyType' | 'xs:anySimpleType' | 'xs:anyAtomicType' | 
                  'xs:untypedAtomic' | 'xs:string' | 'xs:normalizedString' | 
                  'xs:token' | 'xs:language' | 'xs:NMTOKEN' | 'xs:Name' | 
                  'xs:NCName' | 'xs:ID' | 'xs:IDREF' | 'xs:ENTITY' | 'xs:float' | 
                  'xs:double' | 'xs:decimal' | 'xs:precisionDecimal' | 
                  'xs:integer' | 'xs:nonPositiveInteger' | 'xs:negativeInteger' | 
                  'xs:long' | 'xs:int' | 'xs:short' | 'xs:byte' | 
                  'xs:nonNegativeInteger' | 'xs:unsignedLong' | 'xs:unsignedInt' | 
                  'xs:unsignedShort' | 'xs:unsignedByte' | 'xs:positiveInteger' | 
                  'xs:duration' | 'xs:yearMonthDuration' | 'xs:dayTimeDuration' |
                  'xs:dateTime' | 'xs:dateTimeStamp' | 'xs:date' | 'xs:time' | 
                  'xs:gYearMonth' | 'xs:gYear' | 'xs:gMonthDay' | 'xs:gDay' | 
                  'xs:gMonth' | 'xs:boolean' | 'basex:binary' | 
                  'xs:base64Binary' | 'xs:hexBinary' | 'xs:anyURI' | 
                  'xs:QName' | 'xs:NOTATION' | undefined .

-record(context,{
                 value  :: value(),
                 xstype :: xstype()
                }).
-record(context_seq,{
                     values  :: [#context{}]
                    }).

-record(variable,{
                   name   :: name(),
                   value  :: value(),
                   xstype :: xstype()
                  }).
-record(item,{
              value  :: value(),
              xstype :: xstype()
             }).
-record(variable_seq,{
                      name    :: name(),
                      values  :: [#item{}]
                     }).
%% XQuery request
-record(xquery, {
                 statement            :: binary() | string(),
                 context              :: #context{} | #context_seq{} | undefined,
                 variables            :: [#variable{} | #variable_seq{}] | undefined,
                 result_type = single :: single | sequence 
                }).

-record(result, {
                  value  :: binary(),
                  xstype :: xstype()
                }).

%% XQuery response
-record(result_set, {
                     info    :: binary(),
                     results :: [#result{}]
                    }).

%% Database and user names follow the same naming constraints: Names are restricted to ASCII characters. 
%% They must at least have one character, and they may contain letters, numbers and any of the special 
%% characters !#$%&'()+-=@[]^_`{}~. The following characters are reserved for other features:
-type dbname()   :: string().
-type username() :: string().

%% valid directory or file path as string() or binary()
-type path()   :: file:filename_all().
-type uri()    :: string().
-type xml()    :: string() | binary().
-type xquery() :: string() | binary().



%%% ====================
%%% Database Operations
%%% ====================

%% Creates a new database with the specified name and, optionally, an initial input, and opens it. 
%% An existing database will be overwritten.
%% The input can be a file or directory path to XML documents, a remote URL, or a string containing XML:
%% name must be a valid database name
%% database creation can be controlled by setting Create Options
-record(create_db, {
                    name  :: dbname(),
                    input :: path() | uri() | xml() | undefined
                   }).
%% Opens the database specified by name. The documents to be opened can be specified by the [path] argument.
-record(open, {
                    name :: dbname(),
                    path :: path() | undefined
                   }).
%% This convenience command combines OPEN and CREATE DB: if a database with the name input exists, 
%% it is opened. Otherwise, a new database is created; if the specified input points to an existing 
%% resource, it is stored as initial content.
-record(check, {
                    name  :: dbname()
                   }).
%% Closes the currently opened database.
-record(close, {}).
%% Exports all documents in the database to the specified file path, using the serializer options specified by the EXPORTER option.
-record(export, {
                    path :: path()
                   }).
%% Creates the specified Value Index. The current Index Options will be considered when creating the index.
-record(create_index, {
                    value :: text | attribute | token | fulltext
                   }).
%% Drops the specified Value Index.
-record(drop_index, {
                    value :: text | attribute | token | fulltext
                   }).

%%% ====================
%%% Administration
%%% ====================

%% Renames the database specified by name to newname. newname must be a valid database name.
-record(alter_db, {
                   name    :: dbname(),
                   newname :: dbname()
                  }).
%% Drops the database with the specified name. The Glob Syntax can be used to address more than one database.
-record(drop_db, {
                   name    :: dbname()
                  }).
%% Creates a zipped backup of the database specified by name. The backup file will be suffixed with 
%% the current timestamp and stored in the database directory. The Glob Syntax can be used to address more than one database.
-record(create_backup, {
                        name :: dbname()
                       }).
%% Restores a database with the specified name. The name may include the timestamp of the backup file.
-record(restore, {
                        name :: dbname()
                       }).
%% Performs some integrity checks on the opened database and returns a brief summary.
-record(inspect, {}).
%% Drops all backups of the database with the specified name. The Glob Syntax can be used to address more than one database.
-record(drop_backup, {
                        name :: dbname()
                       }).
%% Shows all database backups.
-record(show_backups, {}).
%% Creates a copy of the database specified by name. newname must be a valid database name.
-record(copy, {
               name     :: dbname(),
               newname  :: dbname()
              }).
%% Shows general information and meta data on the currently opened database.
-record(info_db, {}).
%% Shows information on the existing index structures. The output can be optionally limited to the specified index.
-record(info_index, {
                     value :: elemname | attrname | path | text | attribute | token | fulltext
                    }).
%% Shows information on the existing index structures. The output can be optionally limited to the specified index.
-record(info_storage, {
                     range_start :: integer() | undefined,
                     range_end   :: integer() | undefined
                    }).

%%% ====================
%%% Querying
%%% ====================

%% Lists all available databases. If name is specified, the resources of a database are listed. The output can 
%% be further restricted to the resources matching the specified path.
%% If database resources are listed, the size is either the number of nodes (for XML resources) or the number 
%% of bytes (for binary resources).
-record(list, {
               name :: dbname() | undefined,
               path :: path() | undefined
              }).
%% Runs the specified query and returns the result.
-record(sxquery, {
               query :: xquery()
              }).
%% Retrieves a raw file from the opened database at the specified path.
-record(retrieve, {
                   path :: path()
                  }).

%% Builds and runs a query for the specified query terms. Keywords can be enclosed in quotes to look for phrases. 
%% The following modifiers can be used to further limit search:
%% = looks for exact text nodes
%% ~ looks for approximate hits
%% @= looks for exact attribute values
%% @ looks for attributes
-record(find, {
               query :: string()
              }).
%% Runs all XQUnit tests in the specified path. The path can point to a single file or a directory.
-record(test, {
               path :: path()
              }).
%% Installs the *.xar package with path.
-record(repo_install, {
                       path :: path()
                      }).
%% Lists all installed packages.
-record(repo_list, {}).
%% Deletes the specified package with the specified name. What is called "name" can also be the id 
%% (which is the name followed by the version) or the directory of the package.
-record(repo_delete, {
                      name :: dbname()
                     }).

%%% ====================
%%% Updates
%%% ====================

%% Adds a file, directory or XML string specified by input to the currently opened database at the specified path:
%% input may either be a single XML document, a directory, a remote URL or a plain XML string.
%% A document with the same path may occur than once in a database. If this is unwanted, the REPLACE command can be used.
%% If a file is too large to be added in one go, its data structures will be cached to disk first. Caching can be 
%% enforced by turning the ADDCACHE option on.
%% If files are to be added to an empty database, it is usually faster to use the CREATE DB command and specify the 
%% initial input as argument.
-record(add, {
              path  :: path() | undefined,
              input :: path() | uri() | xml()
             }).
%% Deletes all documents from the currently opened database that start with the specified path.
-record(delete, {
                 path :: path()
                }).
%% Renames all document paths in the currently opened database that start with the specified path. 
%% The command may be used to either rename single documents or directories.
-record(rename, {
                 path    :: path(),
                 newpath :: path()
                }).
%% Replaces a document in the currently opened database, addressed by path, with the file or XML 
%% string specified by input, or adds a new document if the resource does not exist yet.
-record(replace, {
                  path  :: path(),
                  input :: path() | xml()
                 }).
%% Stores a raw file specified via input in the opened database to the specified path:
%% The input may either be a file reference, a remote URL, or a plain string.
%% If the path denotes a directory, it needs to be suffixed with a slash (/).
%% An existing resource will be replaced.
-record(store, {
                path  :: path(),
                input :: binary()
               }).
%% Optimizes the index structures, meta data and statistics of the currently opened database:
%% If ALL is specified, all database structures are completely reconstructed. The database size 
%% will be reduced, and all orphaned data will be deleted.
%% Without ALL, only the outdated index structures and database statistics will be updated. If the 
%% database is completely up-to-date, nothing will be done.
%% Database options will be adopted from the original database. Only AUTOOPTIMIZE and (if ALL is specified) 
%% UPDINDEX will be adopted from the current options.
-record(optimize, {
                   all :: boolean()
                  }).
%% Explicitly flushes the buffers of the currently opened database to disk. This command is applied if 
%% AUTOFLUSH has been set to false.
-record(flush, {}).

%%% ====================
%%% Monitoring
%%% ====================

%% Shows all sessions that are connected to the current server instance.
-record(show_sessions, {}).
%% Shows all users that are visible to the current user. If a database is specified, only those users will 
%% be shown for which a pattern was specified that matches the database name.
-record(show_users, {
                     database :: dbname() | undefined
                    }).
%% Kills sessions of a user or an IP:port combination, specified by target. The Glob Syntax can be used to address more than one user.
-record(kill, {
               target :: string()
              }).
%% Returns information on all jobs that are currently queued or executed.
-record(jobs_list, {}).
%% Returns the cached result of a query with the specified job id:
%% Results can only be retrieved once. After retrieval, the cached result will be dropped.
%% If the original query has raised an error, the cached error will be raised instead.
-record(jobs_result, {
                      id :: string()
                     }).
%% Cancels the execution of a job with the specified id, or drops the cached result of a query. 
%% Unknown ids are ignored. All jobs are gracefully stopped; it is up to the process to decide when it is safe to shut down.
-record(jobs_stop, {
                    id :: string()
                   }).

%%% ====================
%%% User Management
%%% ====================

%% Creates a user with the specified name and password. If no password is specified, it is requested via the chosen 
%% frontend (GUI or bash).
-record(create_user, {
                      name     :: username(),
                      password :: string() | undefined
                     }).
%% Renames the user with the specified name to newname.
-record(alter_user, {
                     name    :: username(),
                     newname :: username()
                    }).
%% Alters the password of the user with the specified name. If no password is specified, it is requested via the chosen 
%% frontend (GUI or bash).
-record(alter_password, {
                         name     :: username(),
                         password :: string() | undefined
                        }).
%% Drops the user with the specified name. The Glob Syntax can be used to address more than one database or user. 
%% If a glob pattern is specified, only the pattern will be removed.
-record(drop_user, {
                    name     :: username(),
                    pattern  :: string() | undefined
                   }).
%% Grants the specified permission to the specified user. The Glob Syntax can be used to address more than one user. 
%% If a glob pattern is specified, the permission will be applied to all databases that match this pattern.
-record(grant, {
                right   :: none | read | write | create | admin,
                pattern :: string() | undefined,
                user    :: username()
               }).
%% Changes the password of the current user. If no password is specified, it is requested via the chosen frontend (GUI or bash).
-record(password, {
                   password :: string() | undefined
                  }).

%%% ====================
%%% General Commands
%%% ====================

%% Evaluates the contents of file as XQuery expression. If the file ends with the suffix .bxs, the file 
%% contents will be evaluated as command script. This command can be used to run several commands in a row, 
%% with no other transaction intervening the execution.
-record(run, {
              file :: path()
             }).
%% Evaluates the specified input as command script. This command can be used to run several commands in a row, 
%% with no other transaction intervening the execution.
-record(execute, {
              input :: string() | binary()
             }).
%% Returns the current value of the Option specified via option. 
%% Global options can only be requested by users with ADMIN permissions.
-record(get_option, {
              option :: atom()
             }).
%% Sets the Option specified by option to a new value. Only local options can be modified. 
%% If no value is specified, and if the value is boolean, it will be inverted.
-record(set_option, {
              option :: atom(),
              value  :: string() | integer() | boolean() | binary() | path() | undefined
             }).
%% Returns global information.
-record(info, {}).
%% If command is specified, information on the specific command is printed; otherwise, all commands are listed.
-record(help, {
               command :: string() | undefined
              }).

%%% ====================
%%% Transaction
%%% ====================
-record(transaction, {
                      commands :: [
                                    #add{} |
                                    #alter_db{} |
                                    #alter_password{} |
                                    #alter_user{} |
                                    #check{} |
                                    #close{} |
                                    #copy{} |
                                    #create_backup{} |
                                    #create_db{} |
                                    #create_index{} |
                                    #create_user{} |
                                    #delete{} |
                                    #drop_backup{} |
                                    #drop_db{} |
                                    #drop_index{} |
                                    #drop_user{} |
                                    #execute{} |
                                    #export{} |
                                    #find{} |
                                    #flush{} |
                                    #get_option{} |
                                    #grant{} |
                                    #help{} |
                                    #info{} |
                                    #info_db{} |
                                    #info_index{} |
                                    #info_storage{} |
                                    #inspect{} |
                                    #jobs_list{} |
                                    #jobs_result{} |
                                    #jobs_stop{} |
                                    #kill{} |
                                    #list{} |
                                    #open{} |
                                    #optimize{} |
                                    #password{} |
                                    #rename{} |
                                    #replace{} |
                                    #repo_delete{} |
                                    #repo_install{} |
                                    #repo_list{} |
                                    #restore{} |
                                    #retrieve{} |
                                    #run{} |
                                    #set_option{} |
                                    #show_backups{} |
                                    #show_sessions{} |
                                    #show_users{} |
                                    #store{} |
                                    #sxquery{} |
                                    #test{}
                                   ]
                      }).


