-define(DEBUG(Format, Args),
		    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
		    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
		    lager:warning(Format, Args)).

-define(ERROR(Format, Args),
		    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
		    lager:critical(Format, Args)).
