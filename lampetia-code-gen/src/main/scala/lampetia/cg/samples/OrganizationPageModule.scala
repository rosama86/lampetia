package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module


/**
 * @author Hossam Karim   --  provisioning Fathi Al-Ghaiati
 */



object OrganizationPageModule extends  App {

  val YesNoModel =
    enum("YesNo")("Yes".ecase, "No".ecase)
      .withDiscriminator("value".string) <+ commonFeatures

  // reference Organization

  val OrganizationModel =
    entity("Organization")(
      "name".name
    ).withResourceType("lampetia.model.organization:1.0") <+ commonFeatures

  //Table: OrganizationPageTemplate
  val UrlModel = value("Url")("address".string) <+ commonFeatures

  val OrganizationPackageModel =
    enum("OrganizationPackage")("Public".ecase, "Private".ecase, "Enterprise".ecase)
      .withDiscriminator("packageCode".string) <+ commonFeatures


  val PageTemplateStateModel =
    enum("PageTemplateState")("TemplateCreating".ecase, "TemplateActive".ecase, "TemplateSuspended".ecase , "TemplateDeleted".ecase)
      .withDiscriminator("templateState".string) <+ commonFeatures

  val PageDirectionModel =
    enum("PageDirection")("Default".ecase, "LTR".ecase, "RTL".ecase)
      .withDiscriminator("direction".string) <+ commonFeatures


  val ImageModel =
    composite("Image")("imageTitle".string, "imageUrl".string/* of UrlModel*/) <+ commonFeatures


  val PageTemplateInfoModel =
    composite("PageTemplateInfo")(
      "systemReference".string,
      "direction" optionOf PageDirectionModel,
      "css".string,   // may change
      "images" listOf ImageModel << (jsonArrayComposite in Sql),
      "version".string,
      "description".string
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val OrganizationPageTemplateModel =
    entity("OrganizationPageTemplate")(
      "code".string,
      "packageCode" optionOf OrganizationPackageModel,
      "state" optionOf PageTemplateStateModel,
      ("templateInfo" of PageTemplateInfoModel )<< (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.organization_page_template:1.0") <+ commonFeatures

  //Table OrganizationPage
  val PageStateModel =
    enum("PageState")("PageCreating".ecase, "PageActive".ecase, "PageSuspended".ecase , "PageArchived".ecase, "PageDeleted".ecase)
      .withDiscriminator("pageState".string) <+ commonFeatures

  val KeywordModel =
    composite("Keyword")("value".string) <+ commonFeatures

  /*
  val LinkModel =
    composite("Link")("siteName".string, "siteUrl".string) <+ commonFeatures
  */

  val PageInfoModel =
    composite("PageInfo")(
      "keywords" listOf KeywordModel  << (jsonArrayComposite in Sql),
      "isPeriod" optionOf YesNoModel,
      "startsOn".dateTime,
      "endsOn".dateTime,
      "overview".string,
      "direction" optionOf PageDirectionModel,
      "links" of UrlModel,//listOf UrlModel  << (jsonArrayComposite in Sql),
      "logoUrl" of UrlModel,
      "bannerUrl" of UrlModel
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val OrganizationPageModel =
    entity("OrganizationPage")(
      "title".string,
      "organizationId" ref OrganizationModel.id ,
      "templateId" ref OrganizationPageTemplateModel.id ,
      "state" optionOf PageStateModel,
      ("pageInfo" of PageInfoModel) << (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.organization_page:1.0") <+ commonFeatures

  // Post
  val PostStateModel =
    enum("PostState")("PostCreating".ecase, "PostActive".ecase, "PostSuspended".ecase, "PostDeleted".ecase)
      .withDiscriminator("postState".string) <+ commonFeatures

  val PostLineTypeModel =
    enum("PostLineType")("UrlPostLine".ecase, "PicPostLine".ecase, "TextPostLine".ecase)
      .withDiscriminator("lineType".string) <+ commonFeatures

  val PostLineModel =
    composite("PostLine")(
      //"order".int,
      "lineType" optionOf PostLineTypeModel,
      "value".string
        /*
        --- based on type
              --- case text, value must be Alphanumeric
              --- case pic, value must be url to external store
              --- case url , url value to be on allowed sites
                                          soundcloud,youtube,instagram
        */
    ) <+ commonFeatures

  val PostInfoModel =
    composite("PostInfo")(
      "postLine" listOf PostLineModel  << (jsonArrayComposite in Sql)
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val PostCommentLineModel =
    composite("PostCommentLine")(
      //"order".int,
      "at".dateTime,
      "byUserId".userId,
      "commentText".string
    ) <+ commonFeatures

  val PostCommentsInfoModel =
    composite("PostCommentsInfo")(
      "commentLine" listOf PostCommentLineModel  << (jsonArrayComposite in Sql)
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val OrganizationPagePostModel =
    entity("OrganizationPagePost")(
      "title".string,
      "pageId" ref OrganizationPageModel.id,
      "userId".userId,
      "publishedAt".dateTime,
      "state" optionOf PostStateModel,
      ("postInfo" of PostInfoModel) << (jsonbComposite in Sql),
      ("commentsInfo" of PostCommentsInfoModel) << (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.organization_page_post:1.0") <+ commonFeatures //<<
      //(named("organization_page_post") in Sql)

  //Organization Events (Calendar Based)
  val EventStateModel =
    enum("EventState")("EventCreating".ecase, "EventActive".ecase, "EventSuspended".ecase , "EventDeleted".ecase)
      .withDiscriminator("eventState".string) <+ commonFeatures

  val EventOccurTypeModel =
    enum("EventOccurType")("Onetime".ecase, "Recurring".ecase)
      .withDiscriminator("occurType".string) <+ commonFeatures

  val EventAddressModel =
    composite("EventAddress")(
      "text".string,
      "latitude".string,
      "longitude".string
    ) <+ commonFeatures

  val OneTimeEventModel =
    composite("OneTimeEvent")(
      "startAt".dateTime,
      "FinishAt".dateTime
    ) <+ commonFeatures

  val WeekdayModel =
    enum("Weekday")("SU".ecase, "MO".ecase, "TU".ecase, "WE".ecase, "TH".ecase, "FR".ecase, "SA".ecase)
      .withDiscriminator("weekday".string) <+ commonFeatures

  val RecurringEventLineModel =
    composite("RecurringEventLine")(
      "weekday" optionOf WeekdayModel,
      "startAt".dateTime,
      "FinishAt".dateTime
    ) <+ commonFeatures

  val RecurringEventModel =
    composite("RecurringEvent")(
      "startOn".dateTime,
      "FinishOn".dateTime,
      "occurOnDays" listOf RecurringEventLineModel << (jsonbArrayComposite in Sql)
    ) <+ commonFeatures

  val EventInfoModel =
    composite("EventInfo")(
      "eventAddress" of EventAddressModel,
      "oneTimeEvent" of OneTimeEventModel,
      "recuuringEvent" of RecurringEventModel
    ) <+ commonFeatures << (jsonbComposite in Sql)

  val EventSubscriberStateModel =
    enum("EventSubscriberState")("SubscriberSent".ecase, "SubscriberConfirmed".ecase, "SubscriberTentative".ecase , "SubscriberRejected".ecase)
      .withDiscriminator("eventSubscriberState".string) <+ commonFeatures

  val EventSubscriberModel =
    composite("EventSubscriber")(
      "subscribedAt".dateTime,
      "userId".userId,
      "state" optionOf EventSubscriberStateModel
    ) <+ commonFeatures

  val EventSubscribersModel =
    composite("EventSubscribers")(
      "subscribers" listOf EventSubscriberModel << (jsonbArrayComposite in Sql)
  ) <+ commonFeatures << (jsonbComposite in Sql)

  val OrganizationEventModel =
    entity("OrganizationEvent")(
      "title".string,
      "pageId" ref OrganizationPageModel.id,
      "userId".userId,
      "publishedAt".dateTime,
      "eventType" optionOf EventOccurTypeModel,
      "state" optionOf EventStateModel,
      ("eventInfo" of EventInfoModel) << (jsonbComposite in Sql),
      ("subscribersInfo" of EventSubscribersModel) << (jsonbComposite in Sql)
    ).withResourceType("lampetia.model.organization_event:1.0") <+ commonFeatures //<<
  //(named("organization_event") in Sql)

  lazy val module = Module("OrgPage", "lampetia.organization_page", "org_page") <<
    (dependsOn(OrganizationModule.module.model) in ModelScope) <<
    (dependsOn(OrganizationModule.module.model) in ServiceScope) <<
    (dependsOn(OrganizationModule.module.service) in ServiceScope)


  val models = Seq(
    //page template starts
    //OrganizationPackageModel,
    YesNoModel,
    PageTemplateStateModel,
    PageDirectionModel,
    ImageModel,
    PageTemplateInfoModel,
    OrganizationPageTemplateModel.id.tpe,
    OrganizationPageTemplateModel,

    //page starts
    PageStateModel,
    KeywordModel,
    PageInfoModel,

    OrganizationPageModel.id.tpe,
    OrganizationPageModel,

    // post starts
    //LinkModel,
    PostStateModel,
    PostLineTypeModel,
    PostLineModel,
    PostInfoModel,
    PostCommentLineModel,
    PostCommentsInfoModel,
    OrganizationPagePostModel.id.tpe,
    OrganizationPagePostModel,

    //page events
    EventStateModel,
    EventOccurTypeModel,
    EventAddressModel,
    OneTimeEventModel,
    WeekdayModel,
    RecurringEventLineModel,
    RecurringEventModel,
    EventInfoModel,
    EventSubscriberStateModel,
    EventSubscriberModel,
    EventSubscribersModel,
    OrganizationEventModel.id.tpe,
    OrganizationEventModel)

  CodeGenerator.serviceGenerator(module, models).generate()

}