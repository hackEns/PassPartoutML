module type PARAM = sig
	val welcome_service : (unit, unit, Eliom_service.get_service_kind, Eliom_service.attached,
		Eliom_service.service_kind, [ `WithoutSuffix ], unit, 
		unit, Eliom_service.registrable, [> Eliom_service.appl_service ])
		Eliom_service.service
	include Eliom_registration.APPL_PARAMS
end

module App (M : PARAM) = struct
	include Eliom_registration.App ( M )
	let welcome_service = M.welcome_service
end

module type APP = sig
	include Eliom_registration.ELIOM_APPL
	val welcome_service : (unit, unit, Eliom_service.get_service_kind, Eliom_service.attached,
		Eliom_service.service_kind, [ `WithoutSuffix ], unit, 
		unit, Eliom_service.registrable, [> Eliom_service.appl_service ])
		Eliom_service.service
end
