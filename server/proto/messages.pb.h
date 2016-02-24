// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: messages.proto

#ifndef PROTOBUF_messages_2eproto__INCLUDED
#define PROTOBUF_messages_2eproto__INCLUDED

#include <string>

#include <google/protobuf/stubs/common.h>

#if GOOGLE_PROTOBUF_VERSION < 2006000
#error This file was generated by a newer version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please update
#error your headers.
#endif
#if 2006001 < GOOGLE_PROTOBUF_MIN_PROTOC_VERSION
#error This file was generated by an older version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please
#error regenerate this file with a newer version of protoc.
#endif

#include <google/protobuf/generated_message_util.h>
#include <google/protobuf/message.h>
#include <google/protobuf/repeated_field.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/generated_enum_reflection.h>
#include <google/protobuf/unknown_field_set.h>
// @@protoc_insertion_point(includes)

// Internal implementation detail -- do not call these.
void  protobuf_AddDesc_messages_2eproto();
void protobuf_AssignDesc_messages_2eproto();
void protobuf_ShutdownFile_messages_2eproto();

class Message;
class JoinTableReq;
class JoinTableRes;

enum MessageType {
  JOIN_TABLE_REQ = 1,
  JOIN_TABLE_RES = 2
};
bool MessageType_IsValid(int value);
const MessageType MessageType_MIN = JOIN_TABLE_REQ;
const MessageType MessageType_MAX = JOIN_TABLE_RES;
const int MessageType_ARRAYSIZE = MessageType_MAX + 1;

const ::google::protobuf::EnumDescriptor* MessageType_descriptor();
inline const ::std::string& MessageType_Name(MessageType value) {
  return ::google::protobuf::internal::NameOfEnum(
    MessageType_descriptor(), value);
}
inline bool MessageType_Parse(
    const ::std::string& name, MessageType* value) {
  return ::google::protobuf::internal::ParseNamedEnum<MessageType>(
    MessageType_descriptor(), name, value);
}
// ===================================================================

class Message : public ::google::protobuf::Message {
 public:
  Message();
  virtual ~Message();

  Message(const Message& from);

  inline Message& operator=(const Message& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _unknown_fields_;
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return &_unknown_fields_;
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const Message& default_instance();

  void Swap(Message* other);

  // implements Message ----------------------------------------------

  Message* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const Message& from);
  void MergeFrom(const Message& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const;
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  public:
  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // required .MessageType type = 1;
  inline bool has_type() const;
  inline void clear_type();
  static const int kTypeFieldNumber = 1;
  inline ::MessageType type() const;
  inline void set_type(::MessageType value);

  // optional .JoinTableReq join_table_req = 2;
  inline bool has_join_table_req() const;
  inline void clear_join_table_req();
  static const int kJoinTableReqFieldNumber = 2;
  inline const ::JoinTableReq& join_table_req() const;
  inline ::JoinTableReq* mutable_join_table_req();
  inline ::JoinTableReq* release_join_table_req();
  inline void set_allocated_join_table_req(::JoinTableReq* join_table_req);

  // optional .JoinTableRes join_table_res = 3;
  inline bool has_join_table_res() const;
  inline void clear_join_table_res();
  static const int kJoinTableResFieldNumber = 3;
  inline const ::JoinTableRes& join_table_res() const;
  inline ::JoinTableRes* mutable_join_table_res();
  inline ::JoinTableRes* release_join_table_res();
  inline void set_allocated_join_table_res(::JoinTableRes* join_table_res);

  // @@protoc_insertion_point(class_scope:Message)
 private:
  inline void set_has_type();
  inline void clear_has_type();
  inline void set_has_join_table_req();
  inline void clear_has_join_table_req();
  inline void set_has_join_table_res();
  inline void clear_has_join_table_res();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::JoinTableReq* join_table_req_;
  ::JoinTableRes* join_table_res_;
  int type_;
  friend void  protobuf_AddDesc_messages_2eproto();
  friend void protobuf_AssignDesc_messages_2eproto();
  friend void protobuf_ShutdownFile_messages_2eproto();

  void InitAsDefaultInstance();
  static Message* default_instance_;
};
// -------------------------------------------------------------------

class JoinTableReq : public ::google::protobuf::Message {
 public:
  JoinTableReq();
  virtual ~JoinTableReq();

  JoinTableReq(const JoinTableReq& from);

  inline JoinTableReq& operator=(const JoinTableReq& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _unknown_fields_;
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return &_unknown_fields_;
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const JoinTableReq& default_instance();

  void Swap(JoinTableReq* other);

  // implements Message ----------------------------------------------

  JoinTableReq* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const JoinTableReq& from);
  void MergeFrom(const JoinTableReq& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const;
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  public:
  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // required uint32 table_id = 1;
  inline bool has_table_id() const;
  inline void clear_table_id();
  static const int kTableIdFieldNumber = 1;
  inline ::google::protobuf::uint32 table_id() const;
  inline void set_table_id(::google::protobuf::uint32 value);

  // @@protoc_insertion_point(class_scope:JoinTableReq)
 private:
  inline void set_has_table_id();
  inline void clear_has_table_id();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::google::protobuf::uint32 table_id_;
  friend void  protobuf_AddDesc_messages_2eproto();
  friend void protobuf_AssignDesc_messages_2eproto();
  friend void protobuf_ShutdownFile_messages_2eproto();

  void InitAsDefaultInstance();
  static JoinTableReq* default_instance_;
};
// -------------------------------------------------------------------

class JoinTableRes : public ::google::protobuf::Message {
 public:
  JoinTableRes();
  virtual ~JoinTableRes();

  JoinTableRes(const JoinTableRes& from);

  inline JoinTableRes& operator=(const JoinTableRes& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _unknown_fields_;
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return &_unknown_fields_;
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const JoinTableRes& default_instance();

  void Swap(JoinTableRes* other);

  // implements Message ----------------------------------------------

  JoinTableRes* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const JoinTableRes& from);
  void MergeFrom(const JoinTableRes& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const;
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  public:
  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // required uint32 table_id = 1;
  inline bool has_table_id() const;
  inline void clear_table_id();
  static const int kTableIdFieldNumber = 1;
  inline ::google::protobuf::uint32 table_id() const;
  inline void set_table_id(::google::protobuf::uint32 value);

  // @@protoc_insertion_point(class_scope:JoinTableRes)
 private:
  inline void set_has_table_id();
  inline void clear_has_table_id();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::google::protobuf::uint32 table_id_;
  friend void  protobuf_AddDesc_messages_2eproto();
  friend void protobuf_AssignDesc_messages_2eproto();
  friend void protobuf_ShutdownFile_messages_2eproto();

  void InitAsDefaultInstance();
  static JoinTableRes* default_instance_;
};
// ===================================================================


// ===================================================================

// Message

// required .MessageType type = 1;
inline bool Message::has_type() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void Message::set_has_type() {
  _has_bits_[0] |= 0x00000001u;
}
inline void Message::clear_has_type() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void Message::clear_type() {
  type_ = 1;
  clear_has_type();
}
inline ::MessageType Message::type() const {
  // @@protoc_insertion_point(field_get:Message.type)
  return static_cast< ::MessageType >(type_);
}
inline void Message::set_type(::MessageType value) {
  assert(::MessageType_IsValid(value));
  set_has_type();
  type_ = value;
  // @@protoc_insertion_point(field_set:Message.type)
}

// optional .JoinTableReq join_table_req = 2;
inline bool Message::has_join_table_req() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void Message::set_has_join_table_req() {
  _has_bits_[0] |= 0x00000002u;
}
inline void Message::clear_has_join_table_req() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void Message::clear_join_table_req() {
  if (join_table_req_ != NULL) join_table_req_->::JoinTableReq::Clear();
  clear_has_join_table_req();
}
inline const ::JoinTableReq& Message::join_table_req() const {
  // @@protoc_insertion_point(field_get:Message.join_table_req)
  return join_table_req_ != NULL ? *join_table_req_ : *default_instance_->join_table_req_;
}
inline ::JoinTableReq* Message::mutable_join_table_req() {
  set_has_join_table_req();
  if (join_table_req_ == NULL) join_table_req_ = new ::JoinTableReq;
  // @@protoc_insertion_point(field_mutable:Message.join_table_req)
  return join_table_req_;
}
inline ::JoinTableReq* Message::release_join_table_req() {
  clear_has_join_table_req();
  ::JoinTableReq* temp = join_table_req_;
  join_table_req_ = NULL;
  return temp;
}
inline void Message::set_allocated_join_table_req(::JoinTableReq* join_table_req) {
  delete join_table_req_;
  join_table_req_ = join_table_req;
  if (join_table_req) {
    set_has_join_table_req();
  } else {
    clear_has_join_table_req();
  }
  // @@protoc_insertion_point(field_set_allocated:Message.join_table_req)
}

// optional .JoinTableRes join_table_res = 3;
inline bool Message::has_join_table_res() const {
  return (_has_bits_[0] & 0x00000004u) != 0;
}
inline void Message::set_has_join_table_res() {
  _has_bits_[0] |= 0x00000004u;
}
inline void Message::clear_has_join_table_res() {
  _has_bits_[0] &= ~0x00000004u;
}
inline void Message::clear_join_table_res() {
  if (join_table_res_ != NULL) join_table_res_->::JoinTableRes::Clear();
  clear_has_join_table_res();
}
inline const ::JoinTableRes& Message::join_table_res() const {
  // @@protoc_insertion_point(field_get:Message.join_table_res)
  return join_table_res_ != NULL ? *join_table_res_ : *default_instance_->join_table_res_;
}
inline ::JoinTableRes* Message::mutable_join_table_res() {
  set_has_join_table_res();
  if (join_table_res_ == NULL) join_table_res_ = new ::JoinTableRes;
  // @@protoc_insertion_point(field_mutable:Message.join_table_res)
  return join_table_res_;
}
inline ::JoinTableRes* Message::release_join_table_res() {
  clear_has_join_table_res();
  ::JoinTableRes* temp = join_table_res_;
  join_table_res_ = NULL;
  return temp;
}
inline void Message::set_allocated_join_table_res(::JoinTableRes* join_table_res) {
  delete join_table_res_;
  join_table_res_ = join_table_res;
  if (join_table_res) {
    set_has_join_table_res();
  } else {
    clear_has_join_table_res();
  }
  // @@protoc_insertion_point(field_set_allocated:Message.join_table_res)
}

// -------------------------------------------------------------------

// JoinTableReq

// required uint32 table_id = 1;
inline bool JoinTableReq::has_table_id() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void JoinTableReq::set_has_table_id() {
  _has_bits_[0] |= 0x00000001u;
}
inline void JoinTableReq::clear_has_table_id() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void JoinTableReq::clear_table_id() {
  table_id_ = 0u;
  clear_has_table_id();
}
inline ::google::protobuf::uint32 JoinTableReq::table_id() const {
  // @@protoc_insertion_point(field_get:JoinTableReq.table_id)
  return table_id_;
}
inline void JoinTableReq::set_table_id(::google::protobuf::uint32 value) {
  set_has_table_id();
  table_id_ = value;
  // @@protoc_insertion_point(field_set:JoinTableReq.table_id)
}

// -------------------------------------------------------------------

// JoinTableRes

// required uint32 table_id = 1;
inline bool JoinTableRes::has_table_id() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void JoinTableRes::set_has_table_id() {
  _has_bits_[0] |= 0x00000001u;
}
inline void JoinTableRes::clear_has_table_id() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void JoinTableRes::clear_table_id() {
  table_id_ = 0u;
  clear_has_table_id();
}
inline ::google::protobuf::uint32 JoinTableRes::table_id() const {
  // @@protoc_insertion_point(field_get:JoinTableRes.table_id)
  return table_id_;
}
inline void JoinTableRes::set_table_id(::google::protobuf::uint32 value) {
  set_has_table_id();
  table_id_ = value;
  // @@protoc_insertion_point(field_set:JoinTableRes.table_id)
}


// @@protoc_insertion_point(namespace_scope)

#ifndef SWIG
namespace google {
namespace protobuf {

template <> struct is_proto_enum< ::MessageType> : ::google::protobuf::internal::true_type {};
template <>
inline const EnumDescriptor* GetEnumDescriptor< ::MessageType>() {
  return ::MessageType_descriptor();
}

}  // namespace google
}  // namespace protobuf
#endif  // SWIG

// @@protoc_insertion_point(global_scope)

#endif  // PROTOBUF_messages_2eproto__INCLUDED