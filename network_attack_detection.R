# Import cleaned data 
library(dplyr)
library(ggplot2)
library(effsize)
library(pROC)
library(plotly)
library(VIM)
library(caTools)
library(DataExplorer)
library(randomForest)

unsw_raw = read.csv(
  "/Users/teng1221/Downloads/UNSW-NB15_cleaned_v2.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

nrow(unsw_raw)

df = unsw_raw %>%
  dplyr::select(
    sttl, ct_srv_dst,
    sbytes, dbytes, dttl, dpkts,
    sload, dload, sloss, dloss, dwin, swin,
    label, attack_cat
  )

clean_numeric = function(x) {
  x = as.character(x)
  x = gsub("[^0-9.-]", "", x)
  x[x == ""] = NA
  as.numeric(x)
}

df = hotdeck(df, domain_var = "label", imp_var = FALSE)

dim(df)

plot_missing(df)

df = df$data

df = unsw_raw %>%
  distinct() %>%
  mutate(
    label = as.numeric(label),
    label_factor = factor(label,
                          levels = c(0, 1),
                          labels = c("Normal", "Attack"))
  )


table(df$label_factor)
summary(df$sttl)
summary(df$ct_srv_dst)

# 4-1.1 Descriptive statistics
sttl_status_by_label = df %>%
  group_by(label_factor) %>%
  summarise(
    n          = n(),
    mean_sttl  = mean(sttl),
    median_sttl= median(sttl),
    sd_sttl    = sd(sttl),
    min_sttl   = min(sttl),
    max_sttl   = max(sttl)
  )

ct_status_by_label = df %>%
  group_by(label_factor) %>%
  summarise(
    n          = n(),
    mean_ct    = mean(ct_srv_dst),
    median_ct  = median(ct_srv_dst),
    sd_ct      = sd(ct_srv_dst),
    min_ct     = min(ct_srv_dst),
    max_ct     = max(ct_srv_dst)
  )

sttl_status_by_label
ct_status_by_label

# 4-1.2 Boxplots
ggplot(df, aes(x = label_factor, y = sttl)) +
  geom_boxplot() +
  labs(
    title = "Distribution of sttl by attack label",
    x = "Attack label",
    y = "sttl (source-to-destination TTL)"
  )

ggplot(df, aes(x = label_factor, y = ct_srv_dst)) +
  geom_boxplot() +
  labs(
    title = "Distribution of ct_srv_dst by attack label",
    x = "Attack label",
    y = "ct_srv_dst (connections to same service & destination)"
  )

# 4-1.3 Two-sample tests + effect size
sttl_wilcox_test = wilcox.test(sttl ~ label_factor, data = df)
sttl_t_test      = t.test(sttl ~ label_factor, data = df)

ct_wilcox_test   = wilcox.test(ct_srv_dst ~ label_factor, data = df)
ct_t_test        = t.test(ct_srv_dst ~ label_factor, data = df)

sttl_wilcox_test; sttl_t_test
ct_wilcox_test;  ct_t_test

cohen_sttl = cohen.d(sttl ~ label_factor, data = df)
cohen_ct   = cohen.d(ct_srv_dst ~ label_factor, data = df)

cohen_sttl
cohen_ct

# 4-2. Relationship between sttl & ct_srv_dst
ggplot(df, aes(x = ct_srv_dst, y = sttl, colour = label_factor)) +
  geom_point(alpha = 0.3) +
  labs(
    title  = "Analysis 4-2: Relationship between ct_srv_dst and sttl",
    x      = "ct_srv_dst (connections to same service & destination)",
    y      = "sttl (source-to-destination TTL)",
    colour = "Attack label"
  )

ggplot(df, aes(x = ct_srv_dst, y = sttl, colour = label_factor)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  labs(
    title  = "Analysis 4-2 (log10 scale): ct_srv_dst vs sttl",
    x      = "ct_srv_dst (log10 scale)",
    y      = "sttl",
    colour = "Attack label"
  )

cor_all_pearson  = cor.test(df$ct_srv_dst, df$sttl, method = "pearson")
cor_all_spearman = cor.test(df$ct_srv_dst, df$sttl, method = "spearman")

cor_all_pearson
cor_all_spearman

lm_sttl_ct = lm(sttl ~ ct_srv_dst, data = df)
summary(lm_sttl_ct)

# 4-3. Logistic regression + ROC
clean_numeric = function(x) {
  x = as.character(x)
  x = gsub("[^0-9.-]", "", x)
  x[x == ""] = NA
  as.numeric(x)
}

num_vars = c("sttl","ct_srv_dst","sbytes","dbytes","dttl","dpkts",
             "sload","dload","sloss","dloss","dwin","swin","label")

df[, num_vars] = lapply(df[, num_vars], clean_numeric)

set.seed(123)  
df$label_factor = factor(df$label, levels = c(0, 1))

split = sample.split(df$label, SplitRatio = 0.8)

train_set = subset(df, split == TRUE)   
test_set  = subset(df, split == FALSE)

table(train_set$label)
table(test_set$label)

logit_model = glm(label ~ sttl + ct_srv_dst,
                  data   = train_set,
                  family = binomial)

logit_ext  = glm(label ~ sttl + ct_srv_dst + sbytes + dbytes + dttl + dpkts + sload + dload + 
                   sloss + dloss + dwin + swin,
                 data = train_set, family = binomial)

summary(logit_ext)
summary(logit_model)

set.seed(123)
rf_model = randomForest(
  label_factor ~ sttl + ct_srv_dst + sbytes + dbytes + dpkts,
  data  = train_set,
  ntree = 500,
  importance = TRUE,
)

rf_model

rf_prob = predict(rf_model, test_set, type = "prob")[, "1"]

test_set$class_rf = ifelse(rf_prob >= 0.5, 1, 0)

conf_rf = table(Predicted = test_set$class_rf,
                Actual    = test_set$label)

conf_rf

# Accuracy
acc_rf = sum(diag(conf_rf)) / sum(conf_rf)
acc_rf



test_set$prob_base = predict(logit_model, test_set, type = "response")
test_set$prob_ext  = predict(logit_ext,  test_set, type = "response")

test_set$pred_class = ifelse(test_set$prob_base >= 0.5, 1, 0)
conf_base = table(Predicted = test_set$pred_class,
                  Actual    = test_set$label)

test_set$class_ext = ifelse(test_set$prob_ext >= 0.5, 1, 0)
conf_ext = table(Predicted = test_set$class_ext,
                 Actual    = test_set$label)

conf_base
conf_ext

acc_base = sum(diag(conf_base)) / sum(conf_base)
acc_ext  = sum(diag(conf_ext)) / sum(conf_ext)

acc_base
acc_ext

TP  = conf_base["1","1"]
FN  = conf_base["0","1"]
TN  = conf_base["0","0"]
FP  = conf_base["1","0"]

sens_base = TP / (TP + FN)
spec_base = TN / (TN + FP)

sens_base
spec_base

TP_ext  = conf_ext["1","1"]
FN_ext  = conf_ext["0","1"]
TN_ext  = conf_ext["0","0"]
FP_ext  = conf_ext["1","0"]

sens_ext = TP_ext / (TP_ext + FN_ext)
spec_ext = TN_ext / (TN_ext + FP_ext)

sens_ext
spec_ext

TP_rf  = conf_rf["1","1"]
FN_rf  = conf_rf["0","1"]
TN_rf  = conf_rf["0","0"]
FP_rf  = conf_rf["1","0"]

sens_rf = TP_rf / (TP_rf + FN_rf)
spec_rf = TN_rf / (TN_rf + FP_rf)

sens_rf
spec_rf

roc_base = roc(test_set$label, test_set$prob_base)
roc_ext  = roc(test_set$label, test_set$prob_ext)
roc_rf = roc(test_set$label, rf_prob)

plot(roc_base,
     main = "ROC Curves: Logistic vs Random Forest")
lines(roc_ext, col = "red")
lines(roc_rf,  col = "blue")

legend("bottomright",
       legend = c("Base: sttl + ct_srv_dst",
                  "Extended logistic",
                  "Random Forest (extended features)"),
       col    = c("black", "red", "blue"),
       lwd    = 2)

auc(roc_base)
auc(roc_ext)
auc(roc_rf)

# 4-4 3D scatter
df = df %>%
  dplyr::select(sttl, ct_srv_dst, sbytes, label, attack_cat) %>%
  distinct() %>%
  mutate(
    label = as.numeric(label),
    label_factor = factor(label, levels = c(0,1),
                          labels = c("Normal","Attack"))
  )

# 4-4.1 3D scatter
plot_ly(
  df,
  x = ~ct_srv_dst,
  y = ~sttl,
  z = ~sbytes,
  color = ~label_factor,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.6)
) %>%
  layout(
    title = "3D relationship among ct_srv_dst, sttl and sbytes by attack label",
    scene = list(
      xaxis = list(title = "ct_srv_dst"),
      yaxis = list(title = "sttl"),
      zaxis = list(title = "sbytes (source bytes)")
    )
  )

# 4-4.2 sbytes by label
sbytes_by_label = df %>%
  group_by(label_factor) %>%
  summarise(
    n           = n(),
    mean_sbytes = mean(sbytes),
    median_sbytes = median(sbytes),
    sd_sbytes   = sd(sbytes),
    IQR_sbytes  = IQR(sbytes),
    min_sbytes  = min(sbytes),
    max_sbytes  = max(sbytes)
  )

sbytes_by_label

ggplot(df, aes(x = label_factor, y = sbytes)) +
  geom_boxplot() +
  labs(
    title = "Distribution of sbytes by attack label",
    x = "Attack label",
    y = "sbytes (source bytes)"
  )


AIC(logit_base, logit_ext)
summary(logit_ext)

