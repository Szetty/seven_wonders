// This file is generated by rust-protobuf 2.22.0. Do not edit
// @generated

// https://github.com/rust-lang/rust-clippy/issues/702
#![allow(unknown_lints)]
#![allow(clippy::all)]

#![allow(unused_attributes)]
#![rustfmt::skip]

#![allow(box_pointers)]
#![allow(dead_code)]
#![allow(missing_docs)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(trivial_casts)]
#![allow(unused_imports)]
#![allow(unused_results)]
//! Generated file from `start_game.proto`

/// Generated files are compatible only with the same version
/// of protobuf runtime.
// const _PROTOBUF_VERSION_CHECK: () = ::protobuf::VERSION_2_22_0;

#[derive(PartialEq,Clone,Default)]
pub struct Request {
    // message fields
    pub players: ::protobuf::RepeatedField<::std::string::String>,
    pub wonder_sides: ::protobuf::RepeatedField<Request_WonderSide>,
    // special fields
    pub unknown_fields: ::protobuf::UnknownFields,
    pub cached_size: ::protobuf::CachedSize,
}

impl<'a> ::std::default::Default for &'a Request {
    fn default() -> &'a Request {
        <Request as ::protobuf::Message>::default_instance()
    }
}

impl Request {
    pub fn new() -> Request {
        ::std::default::Default::default()
    }

    // repeated string players = 1;


    pub fn get_players(&self) -> &[::std::string::String] {
        &self.players
    }
    pub fn clear_players(&mut self) {
        self.players.clear();
    }

    // Param is passed by value, moved
    pub fn set_players(&mut self, v: ::protobuf::RepeatedField<::std::string::String>) {
        self.players = v;
    }

    // Mutable pointer to the field.
    pub fn mut_players(&mut self) -> &mut ::protobuf::RepeatedField<::std::string::String> {
        &mut self.players
    }

    // Take field
    pub fn take_players(&mut self) -> ::protobuf::RepeatedField<::std::string::String> {
        ::std::mem::replace(&mut self.players, ::protobuf::RepeatedField::new())
    }

    // repeated .core.api.start_game.Request.WonderSide wonder_sides = 2;


    pub fn get_wonder_sides(&self) -> &[Request_WonderSide] {
        &self.wonder_sides
    }
    pub fn clear_wonder_sides(&mut self) {
        self.wonder_sides.clear();
    }

    // Param is passed by value, moved
    pub fn set_wonder_sides(&mut self, v: ::protobuf::RepeatedField<Request_WonderSide>) {
        self.wonder_sides = v;
    }

    // Mutable pointer to the field.
    pub fn mut_wonder_sides(&mut self) -> &mut ::protobuf::RepeatedField<Request_WonderSide> {
        &mut self.wonder_sides
    }

    // Take field
    pub fn take_wonder_sides(&mut self) -> ::protobuf::RepeatedField<Request_WonderSide> {
        ::std::mem::replace(&mut self.wonder_sides, ::protobuf::RepeatedField::new())
    }
}

impl ::protobuf::Message for Request {
    fn is_initialized(&self) -> bool {
        for v in &self.wonder_sides {
            if !v.is_initialized() {
                return false;
            }
        };
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_repeated_string_into(wire_type, is, &mut self.players)?;
                },
                2 => {
                    ::protobuf::rt::read_repeated_message_into(wire_type, is, &mut self.wonder_sides)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        for value in &self.players {
            my_size += ::protobuf::rt::string_size(1, &value);
        };
        for value in &self.wonder_sides {
            let len = value.compute_size();
            my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
        };
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::ProtobufResult<()> {
        for v in &self.players {
            os.write_string(1, &v)?;
        };
        for v in &self.wonder_sides {
            os.write_tag(2, ::protobuf::wire_format::WireTypeLengthDelimited)?;
            os.write_raw_varint32(v.get_cached_size())?;
            v.write_to_with_cached_sizes(os)?;
        };
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &dyn (::std::any::Any) {
        self as &dyn (::std::any::Any)
    }
    fn as_any_mut(&mut self) -> &mut dyn (::std::any::Any) {
        self as &mut dyn (::std::any::Any)
    }
    fn into_any(self: ::std::boxed::Box<Self>) -> ::std::boxed::Box<dyn (::std::any::Any)> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        Self::descriptor_static()
    }

    fn new() -> Request {
        Request::new()
    }

    fn descriptor_static() -> &'static ::protobuf::reflect::MessageDescriptor {
        static descriptor: ::protobuf::rt::LazyV2<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::LazyV2::INIT;
        descriptor.get(|| {
            let mut fields = ::std::vec::Vec::new();
            fields.push(::protobuf::reflect::accessor::make_repeated_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                "players",
                |m: &Request| { &m.players },
                |m: &mut Request| { &mut m.players },
            ));
            fields.push(::protobuf::reflect::accessor::make_repeated_field_accessor::<_, ::protobuf::types::ProtobufTypeMessage<Request_WonderSide>>(
                "wonder_sides",
                |m: &Request| { &m.wonder_sides },
                |m: &mut Request| { &mut m.wonder_sides },
            ));
            ::protobuf::reflect::MessageDescriptor::new_pb_name::<Request>(
                "Request",
                fields,
                file_descriptor_proto()
            )
        })
    }

    fn default_instance() -> &'static Request {
        static instance: ::protobuf::rt::LazyV2<Request> = ::protobuf::rt::LazyV2::INIT;
        instance.get(Request::new)
    }
}

impl ::protobuf::Clear for Request {
    fn clear(&mut self) {
        self.players.clear();
        self.wonder_sides.clear();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for Request {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for Request {
    fn as_ref(&self) -> ::protobuf::reflect::ReflectValueRef {
        ::protobuf::reflect::ReflectValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct Request_WonderSide {
    // message fields
    wonder_name: ::protobuf::SingularField<::std::string::String>,
    is_side_b: ::std::option::Option<bool>,
    // special fields
    pub unknown_fields: ::protobuf::UnknownFields,
    pub cached_size: ::protobuf::CachedSize,
}

impl<'a> ::std::default::Default for &'a Request_WonderSide {
    fn default() -> &'a Request_WonderSide {
        <Request_WonderSide as ::protobuf::Message>::default_instance()
    }
}

impl Request_WonderSide {
    pub fn new() -> Request_WonderSide {
        ::std::default::Default::default()
    }

    // optional string wonder_name = 1;


    pub fn get_wonder_name(&self) -> &str {
        match self.wonder_name.as_ref() {
            Some(v) => &v,
            None => "",
        }
    }
    pub fn clear_wonder_name(&mut self) {
        self.wonder_name.clear();
    }

    pub fn has_wonder_name(&self) -> bool {
        self.wonder_name.is_some()
    }

    // Param is passed by value, moved
    pub fn set_wonder_name(&mut self, v: ::std::string::String) {
        self.wonder_name = ::protobuf::SingularField::some(v);
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_wonder_name(&mut self) -> &mut ::std::string::String {
        if self.wonder_name.is_none() {
            self.wonder_name.set_default();
        }
        self.wonder_name.as_mut().unwrap()
    }

    // Take field
    pub fn take_wonder_name(&mut self) -> ::std::string::String {
        self.wonder_name.take().unwrap_or_else(|| ::std::string::String::new())
    }

    // optional bool is_side_b = 2;


    pub fn get_is_side_b(&self) -> bool {
        self.is_side_b.unwrap_or(false)
    }
    pub fn clear_is_side_b(&mut self) {
        self.is_side_b = ::std::option::Option::None;
    }

    pub fn has_is_side_b(&self) -> bool {
        self.is_side_b.is_some()
    }

    // Param is passed by value, moved
    pub fn set_is_side_b(&mut self, v: bool) {
        self.is_side_b = ::std::option::Option::Some(v);
    }
}

impl ::protobuf::Message for Request_WonderSide {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_string_into(wire_type, is, &mut self.wonder_name)?;
                },
                2 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_bool()?;
                    self.is_side_b = ::std::option::Option::Some(tmp);
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if let Some(ref v) = self.wonder_name.as_ref() {
            my_size += ::protobuf::rt::string_size(1, &v);
        }
        if let Some(v) = self.is_side_b {
            my_size += 2;
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::ProtobufResult<()> {
        if let Some(ref v) = self.wonder_name.as_ref() {
            os.write_string(1, &v)?;
        }
        if let Some(v) = self.is_side_b {
            os.write_bool(2, v)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &dyn (::std::any::Any) {
        self as &dyn (::std::any::Any)
    }
    fn as_any_mut(&mut self) -> &mut dyn (::std::any::Any) {
        self as &mut dyn (::std::any::Any)
    }
    fn into_any(self: ::std::boxed::Box<Self>) -> ::std::boxed::Box<dyn (::std::any::Any)> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        Self::descriptor_static()
    }

    fn new() -> Request_WonderSide {
        Request_WonderSide::new()
    }

    fn descriptor_static() -> &'static ::protobuf::reflect::MessageDescriptor {
        static descriptor: ::protobuf::rt::LazyV2<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::LazyV2::INIT;
        descriptor.get(|| {
            let mut fields = ::std::vec::Vec::new();
            fields.push(::protobuf::reflect::accessor::make_singular_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                "wonder_name",
                |m: &Request_WonderSide| { &m.wonder_name },
                |m: &mut Request_WonderSide| { &mut m.wonder_name },
            ));
            fields.push(::protobuf::reflect::accessor::make_option_accessor::<_, ::protobuf::types::ProtobufTypeBool>(
                "is_side_b",
                |m: &Request_WonderSide| { &m.is_side_b },
                |m: &mut Request_WonderSide| { &mut m.is_side_b },
            ));
            ::protobuf::reflect::MessageDescriptor::new_pb_name::<Request_WonderSide>(
                "Request.WonderSide",
                fields,
                file_descriptor_proto()
            )
        })
    }

    fn default_instance() -> &'static Request_WonderSide {
        static instance: ::protobuf::rt::LazyV2<Request_WonderSide> = ::protobuf::rt::LazyV2::INIT;
        instance.get(Request_WonderSide::new)
    }
}

impl ::protobuf::Clear for Request_WonderSide {
    fn clear(&mut self) {
        self.wonder_name.clear();
        self.is_side_b = ::std::option::Option::None;
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for Request_WonderSide {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for Request_WonderSide {
    fn as_ref(&self) -> ::protobuf::reflect::ReflectValueRef {
        ::protobuf::reflect::ReflectValueRef::Message(self)
    }
}

static file_descriptor_proto_data: &'static [u8] = b"\
    \n\x10start_game.proto\x12\x13core.api.start_game\"\xba\x01\n\x07Request\
    \x12\x18\n\x07players\x18\x01\x20\x03(\tR\x07players\x12J\n\x0cwonder_si\
    des\x18\x02\x20\x03(\x0b2'.core.api.start_game.Request.WonderSideR\x0bwo\
    nderSides\x1aI\n\nWonderSide\x12\x1f\n\x0bwonder_name\x18\x01\x20\x01(\t\
    R\nwonderName\x12\x1a\n\tis_side_b\x18\x02\x20\x01(\x08R\x07isSideBB\x06\
    Z\x04coreJ\x8d\x03\n\x06\x12\x04\0\0\x0c\x01\n\x08\n\x01\x0c\x12\x03\0\0\
    \x12\n\x08\n\x01\x02\x12\x03\x02\0\x1c\n\x08\n\x01\x08\x12\x03\x03\0\x1b\
    \n\t\n\x02\x08\x0b\x12\x03\x03\0\x1b\n\n\n\x02\x04\0\x12\x04\x05\0\x0c\
    \x01\n\n\n\x03\x04\0\x01\x12\x03\x05\x08\x0f\n\x0b\n\x04\x04\0\x02\0\x12\
    \x03\x06\x02\x1e\n\x0c\n\x05\x04\0\x02\0\x04\x12\x03\x06\x02\n\n\x0c\n\
    \x05\x04\0\x02\0\x05\x12\x03\x06\x0b\x11\n\x0c\n\x05\x04\0\x02\0\x01\x12\
    \x03\x06\x12\x19\n\x0c\n\x05\x04\0\x02\0\x03\x12\x03\x06\x1c\x1d\n\x0c\n\
    \x04\x04\0\x03\0\x12\x04\x07\x02\n\x03\n\x0c\n\x05\x04\0\x03\0\x01\x12\
    \x03\x07\n\x14\n\r\n\x06\x04\0\x03\0\x02\0\x12\x03\x08\x04$\n\x0e\n\x07\
    \x04\0\x03\0\x02\0\x04\x12\x03\x08\x04\x0c\n\x0e\n\x07\x04\0\x03\0\x02\0\
    \x05\x12\x03\x08\r\x13\n\x0e\n\x07\x04\0\x03\0\x02\0\x01\x12\x03\x08\x14\
    \x1f\n\x0e\n\x07\x04\0\x03\0\x02\0\x03\x12\x03\x08\"#\n\r\n\x06\x04\0\
    \x03\0\x02\x01\x12\x03\t\x04\x20\n\x0e\n\x07\x04\0\x03\0\x02\x01\x04\x12\
    \x03\t\x04\x0c\n\x0e\n\x07\x04\0\x03\0\x02\x01\x05\x12\x03\t\r\x11\n\x0e\
    \n\x07\x04\0\x03\0\x02\x01\x01\x12\x03\t\x12\x1b\n\x0e\n\x07\x04\0\x03\0\
    \x02\x01\x03\x12\x03\t\x1e\x1f\n\x0b\n\x04\x04\0\x02\x01\x12\x03\x0b\x02\
    '\n\x0c\n\x05\x04\0\x02\x01\x04\x12\x03\x0b\x02\n\n\x0c\n\x05\x04\0\x02\
    \x01\x06\x12\x03\x0b\x0b\x15\n\x0c\n\x05\x04\0\x02\x01\x01\x12\x03\x0b\
    \x16\"\n\x0c\n\x05\x04\0\x02\x01\x03\x12\x03\x0b%&\
";

static file_descriptor_proto_lazy: ::protobuf::rt::LazyV2<::protobuf::descriptor::FileDescriptorProto> = ::protobuf::rt::LazyV2::INIT;

fn parse_descriptor_proto() -> ::protobuf::descriptor::FileDescriptorProto {
    ::protobuf::Message::parse_from_bytes(file_descriptor_proto_data).unwrap()
}

pub fn file_descriptor_proto() -> &'static ::protobuf::descriptor::FileDescriptorProto {
    file_descriptor_proto_lazy.get(|| {
        parse_descriptor_proto()
    })
}