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
//! Generated file from `game_settings.proto`

/// Generated files are compatible only with the same version
/// of protobuf runtime.
// const _PROTOBUF_VERSION_CHECK: () = ::protobuf::VERSION_2_22_0;

#[derive(PartialEq,Clone,Default)]
pub struct Reply {
    // message fields
    version: ::protobuf::SingularField<::std::string::String>,
    pub supported_wonders: ::protobuf::RepeatedField<::std::string::String>,
    // special fields
    pub unknown_fields: ::protobuf::UnknownFields,
    pub cached_size: ::protobuf::CachedSize,
}

impl<'a> ::std::default::Default for &'a Reply {
    fn default() -> &'a Reply {
        <Reply as ::protobuf::Message>::default_instance()
    }
}

impl Reply {
    pub fn new() -> Reply {
        ::std::default::Default::default()
    }

    // optional string version = 1;


    pub fn get_version(&self) -> &str {
        match self.version.as_ref() {
            Some(v) => &v,
            None => "",
        }
    }
    pub fn clear_version(&mut self) {
        self.version.clear();
    }

    pub fn has_version(&self) -> bool {
        self.version.is_some()
    }

    // Param is passed by value, moved
    pub fn set_version(&mut self, v: ::std::string::String) {
        self.version = ::protobuf::SingularField::some(v);
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_version(&mut self) -> &mut ::std::string::String {
        if self.version.is_none() {
            self.version.set_default();
        }
        self.version.as_mut().unwrap()
    }

    // Take field
    pub fn take_version(&mut self) -> ::std::string::String {
        self.version.take().unwrap_or_else(|| ::std::string::String::new())
    }

    // repeated string supported_wonders = 2;


    pub fn get_supported_wonders(&self) -> &[::std::string::String] {
        &self.supported_wonders
    }
    pub fn clear_supported_wonders(&mut self) {
        self.supported_wonders.clear();
    }

    // Param is passed by value, moved
    pub fn set_supported_wonders(&mut self, v: ::protobuf::RepeatedField<::std::string::String>) {
        self.supported_wonders = v;
    }

    // Mutable pointer to the field.
    pub fn mut_supported_wonders(&mut self) -> &mut ::protobuf::RepeatedField<::std::string::String> {
        &mut self.supported_wonders
    }

    // Take field
    pub fn take_supported_wonders(&mut self) -> ::protobuf::RepeatedField<::std::string::String> {
        ::std::mem::replace(&mut self.supported_wonders, ::protobuf::RepeatedField::new())
    }
}

impl ::protobuf::Message for Reply {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream<'_>) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_string_into(wire_type, is, &mut self.version)?;
                },
                2 => {
                    ::protobuf::rt::read_repeated_string_into(wire_type, is, &mut self.supported_wonders)?;
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
        if let Some(ref v) = self.version.as_ref() {
            my_size += ::protobuf::rt::string_size(1, &v);
        }
        for value in &self.supported_wonders {
            my_size += ::protobuf::rt::string_size(2, &value);
        };
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream<'_>) -> ::protobuf::ProtobufResult<()> {
        if let Some(ref v) = self.version.as_ref() {
            os.write_string(1, &v)?;
        }
        for v in &self.supported_wonders {
            os.write_string(2, &v)?;
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

    fn new() -> Reply {
        Reply::new()
    }

    fn descriptor_static() -> &'static ::protobuf::reflect::MessageDescriptor {
        static descriptor: ::protobuf::rt::LazyV2<::protobuf::reflect::MessageDescriptor> = ::protobuf::rt::LazyV2::INIT;
        descriptor.get(|| {
            let mut fields = ::std::vec::Vec::new();
            fields.push(::protobuf::reflect::accessor::make_singular_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                "version",
                |m: &Reply| { &m.version },
                |m: &mut Reply| { &mut m.version },
            ));
            fields.push(::protobuf::reflect::accessor::make_repeated_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                "supported_wonders",
                |m: &Reply| { &m.supported_wonders },
                |m: &mut Reply| { &mut m.supported_wonders },
            ));
            ::protobuf::reflect::MessageDescriptor::new_pb_name::<Reply>(
                "Reply",
                fields,
                file_descriptor_proto()
            )
        })
    }

    fn default_instance() -> &'static Reply {
        static instance: ::protobuf::rt::LazyV2<Reply> = ::protobuf::rt::LazyV2::INIT;
        instance.get(Reply::new)
    }
}

impl ::protobuf::Clear for Reply {
    fn clear(&mut self) {
        self.version.clear();
        self.supported_wonders.clear();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for Reply {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for Reply {
    fn as_ref(&self) -> ::protobuf::reflect::ReflectValueRef {
        ::protobuf::reflect::ReflectValueRef::Message(self)
    }
}

static file_descriptor_proto_data: &'static [u8] = b"\
    \n\x13game_settings.proto\x12\x16core.api.game_settings\"N\n\x05Reply\
    \x12\x18\n\x07version\x18\x01\x20\x01(\tR\x07version\x12+\n\x11supported\
    _wonders\x18\x02\x20\x03(\tR\x10supportedWondersB\x06Z\x04coreJ\xd3\x01\
    \n\x06\x12\x04\0\0\x08\x01\n\x08\n\x01\x0c\x12\x03\0\0\x12\n\x08\n\x01\
    \x02\x12\x03\x02\0\x1f\n\x08\n\x01\x08\x12\x03\x03\0\x1b\n\t\n\x02\x08\
    \x0b\x12\x03\x03\0\x1b\n\n\n\x02\x04\0\x12\x04\x05\0\x08\x01\n\n\n\x03\
    \x04\0\x01\x12\x03\x05\x08\r\n\x0b\n\x04\x04\0\x02\0\x12\x03\x06\x02\x1e\
    \n\x0c\n\x05\x04\0\x02\0\x04\x12\x03\x06\x02\n\n\x0c\n\x05\x04\0\x02\0\
    \x05\x12\x03\x06\x0b\x11\n\x0c\n\x05\x04\0\x02\0\x01\x12\x03\x06\x12\x19\
    \n\x0c\n\x05\x04\0\x02\0\x03\x12\x03\x06\x1c\x1d\n\x0b\n\x04\x04\0\x02\
    \x01\x12\x03\x07\x02(\n\x0c\n\x05\x04\0\x02\x01\x04\x12\x03\x07\x02\n\n\
    \x0c\n\x05\x04\0\x02\x01\x05\x12\x03\x07\x0b\x11\n\x0c\n\x05\x04\0\x02\
    \x01\x01\x12\x03\x07\x12#\n\x0c\n\x05\x04\0\x02\x01\x03\x12\x03\x07&'\
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
