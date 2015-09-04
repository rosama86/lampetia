package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

object LogsBoxModule extends App { // <-- extends App

  val UrlModel = value("Url")("address".string) <+ commonFeatures

  val AdressingTitle =
    enum("AdressingTitle")("Mr", "Ms","Mrs")
      .withDiscriminator("state".string) <+ commonFeatures

  val PublicTag = composite("PublicTag")(
    "tag".title
  ) <+ commonFeatures <<
    (jsonbComposite in Sql) <<
    (named("public_tag") in Sql)

  val CountryModel = entity("Country")(
    "name".name
  ) <+ commonFeatures

  val AddressModel = composite("Address")(
    "countryId" ref CountryModel.id,
    "city".string,
    "addressLine".string
  ) <+ commonFeatures << (jsonbComposite in Sql)

  val ContactModel = composite("Contact")(
    "name".title,
    "email".email,
    "phone".string
  ) << (jsonbComposite in Sql)

  val Company = entity("Company")(
    "tags".jsonb ,
    "title".title,
    "domain" of UrlModel,
    ("address" of AddressModel) << (jsonbComposite in Sql),
    "phone".string,
    "fax".string,
    "publicEmail".string,
    "avatar" of UrlModel,
    "verified".boolean,
    "verificationKey".string,
    "VerifiedOn".dateTime
  ) <+ commonFeatures

  val SubDomain = entity("SubDomain")(
    "companyId" parentRef Company.id,
    "title".title,
    "code".string
  )<+ commonFeatures <<
    (named("sub_domain") in Sql)

  val CloudDriveInfoModel = composite("CloudDriveInfo")(
    "drivepath".string,
    "username".string,
    "password".string
  ) << (jsonbComposite in Sql)

  val LogsBox = entity("LogsBox")(
    "companyId" ref Company.id,
    "subdomainId" parentRef SubDomain.id,
    "title".title,
    "avatar".string,
    "isPublic".boolean,
    ("drivePathInfo" of CloudDriveInfoModel) << (jsonbComposite in Sql)
  )<+ commonFeatures


  val Client = entity("Client")(
    "tags".jsonb ,
    "title".title,
    "wepSite" of UrlModel,
    ("address" of AddressModel) << (jsonbComposite in Sql),
    "countryId" parentRef CountryModel.id,
    "phone".string,
    "fax".string,
    "publicEmail".string,
    "avatar" of UrlModel,
    ("contacts" of ContactModel) << (jsonbComposite in Sql)
  ) <+ commonFeatures

  val DeliveryMethod =
    enum("DeliveryMethod")("Mail", "Telephone","Fax","LogsBoxDelivery")
      .withDiscriminator("state".string) <+ commonFeatures

  val LogClientInfo = composite("LogClientInfo")(
    "method" of DeliveryMethod,
    "clientId" optionalRef Client.id,
    "clientName".title,
    "clientAddress".string,
    "clientContactPerson".string,
    "clientTel".string,
    "clientFaxNo".string,
    "companyId" optionalRef Company.id,
    "subdomainId" optionalRef SubDomain.id,
    "logsboxId" optionalRef LogsBox.id

  ) << (jsonbComposite in Sql) <+ commonFeatures



  val UserInvitation = entity("UserInvitation")(
    "companyId" ref Company.id,
    "subdomainId" parentRef SubDomain.id,
    "box" parentRef LogsBox.id,
    "userEmail" of UrlModel,
    "drafter".boolean,
    "action".boolean,
    "invitedOn".dateTime,
    "acceptedOn".dateTime,
    "rejectedOn".dateTime,
    "stoppedOn".dateTime,
    "userId".string

  )
  /*
    // security module provides needed authority, not sure we need a userId or email here
    val LogsBoxAuthorization = entity("LogsBoxAuthorization")(
      "companyId" ref Company.id,
      "subdomainId" parentRef SubDomain.id,
      "box" parentRef LogsBox.id,
      //"userEmail" of UrlModel,
      "drafter".boolean,
      "action".boolean,
      "acceptedOn".dateTime
      //"userId".userId
    )
  */
  val LogType =
    enum("LogType")("In", "Out")
      .withDiscriminator("state".string) <+ commonFeatures


  val LogsBoxDriveModel = composite("LogsBoxDriveModel")(
    "drivepath".string,
    "username".string,
    "password".string
  )

  val LogAction =
    enum("LogAction")("Drafting", "Sent","Deleted","Hold","Rejected","Closed","InProgress","Replied")
      .withDiscriminator("state".string) <+ commonFeatures

  val AttachmentModel = composite("AttachmentModel")(
    "attachment" of UrlModel,
    "attachFileName".string,
    "attachmentTile".title,
    "uploadedOn".dateTime,
    "uploadedBy".userId,
    "ploadedByUserName".string
  ) << (jsonbArrayComposite in Sql)

  val LogHistoryModel = composite("LogHistoryModel")(
    "noteByUser".userId,
    "noteBySystem".string,
    "on".dateTime,
    "text".dateTime,
    "actionType".string
  ) << (jsonbArrayComposite in Sql)


  val Log = entity("Log")(
    "logType" of LogType,
    "deliveryMethod" of DeliveryMethod,
    "boxId" parentRef LogsBox.id,
    ("clientInfo" of LogClientInfo) << (jsonbComposite in Sql),
    ("tags" of PublicTag) << (jsonbComposite in Sql),
    "subject".string,
    "generatedRef".string,
    "createdOn".dateTime,
    "lastUpdate".dateTime,
    "sent".dateTime,
    "lastActionOn".dateTime,
    "lastAction" of LogAction,
    "messge".string,
    ("attachemnts" listOf AttachmentModel) << (jsonbArrayComposite in Sql),
    ("notes" listOf LogHistoryModel) << (jsonbArrayComposite in Sql)
  )

  val LogShare = entity("LogShare")(
    "logId" parentRef Log.id,
    "logsBoxId" ref LogsBox.id,
    "emptycol".string
  )

  val models = Seq(
    UrlModel,
    AdressingTitle,
    PublicTag,
    CountryModel.id.tpe,
    CountryModel,
    AddressModel,
    ContactModel,
    Company.id.tpe,
    Company,
    SubDomain.id.tpe,
    SubDomain,
    CloudDriveInfoModel,
    LogsBox.id.tpe,
    LogsBox,
    Client.id.tpe,
    Client,
    LogClientInfo,
    UserInvitation.id.tpe,
    UserInvitation,
    //   LogsBoxAuthorization.id.tpe,
    //   LogsBoxAuthorization,
    LogType,
    DeliveryMethod,
    LogAction,
    AttachmentModel,
    LogHistoryModel,
    Log.id.tpe,
    Log,
    LogShare.id.tpe,
    LogShare
  )

  lazy val module = Module("LogsBox", "lampetia.LogsBox", "lampetia")

  CodeGenerator.serviceGenerator(module, models).generate()
  CodeGenerator.scriptGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.nodeGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.deployGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.utilGenerator(module, models).generate(formatScalaSource = false)

}
